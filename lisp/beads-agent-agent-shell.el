;;; beads-agent-agent-shell.el --- agent-shell backend for beads-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the agent-shell backend for beads-agent.
;; It integrates with xenodium's agent-shell package to enable
;; AI-assisted issue work through various AI agents.
;;
;; agent-shell is an Emacs package that provides AI agent capabilities
;; through shell integration.  It supports multiple AI backends and
;; features like file capabilities, session modes, and MCP servers.
;; See: https://github.com/xenodium/agent-shell
;;
;; The backend implements all required protocol methods:
;; - beads-agent-backend-available-p
;; - beads-agent-backend-start
;; - beads-agent-backend-stop
;; - beads-agent-backend-session-active-p
;; - beads-agent-backend-switch-to-buffer
;; - beads-agent-backend-send-prompt
;; - beads-agent-backend-get-buffer
;;
;; Usage:
;;   The backend is automatically registered when this file is loaded.
;;   Ensure agent-shell is installed from:
;;   https://github.com/xenodium/agent-shell
;;
;; Buffer identification:
;;   agent-shell buffers are identified by having `agent-shell-mode' and
;;   a matching `default-directory'.

;;; Code:

(require 'beads-agent-backend)
(require 'seq)

;;; External Function Declarations (PUBLIC API ONLY)

;; From agent-shell.el - session management
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")

;; From acp.el - client shutdown
(declare-function acp-shutdown "acp")

;; From shell-maker - buffer resolution / output (see the compatibility
;; section at the bottom of this file).
(declare-function shell-maker-buffer "shell-maker" (config))
(declare-function shell-maker-process-name "shell-maker" (config))
(declare-function shell-maker-finish-output "shell-maker"
                  (&key config success on-output))
(defvar shell-maker--config)

;; Declare external variables
(defvar agent-shell-agent-configs)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell--state)

;;; Customization

(defgroup beads-agent-agent-shell nil
  "Agent-shell backend for beads-agent."
  :group 'beads-agent
  :prefix "beads-agent-agent-shell-")

(defcustom beads-agent-agent-shell-config nil
  "Agent config to use for beads-agent sessions.
If nil, uses `agent-shell-preferred-agent-config' if set,
otherwise prompts user to select from `agent-shell-agent-configs'.
See `agent-shell-make-agent-config' for config format."
  :type '(choice (const :tag "Use preferred or prompt" nil)
                 (alist :tag "Agent config"))
  :group 'beads-agent-agent-shell)

;;; Helper Functions

(defun beads-agent-agent-shell--find-buffers (dir)
  "Find agent-shell buffers for directory DIR.
Returns list of buffers in `agent-shell-mode' with matching `default-directory'."
  (let ((normalized-dir (directory-file-name
                         (file-truename (expand-file-name dir)))))
    (seq-filter
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (and (derived-mode-p 'agent-shell-mode)
                (string= (directory-file-name
                          (file-truename (expand-file-name default-directory)))
                         normalized-dir)))))
     (buffer-list))))

(defun beads-agent-agent-shell--buffer-active-p (buf)
  "Return non-nil if BUF is an active agent-shell session."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (and (derived-mode-p 'agent-shell-mode)
              (get-buffer-process buf)))))

;;; Backend Class

(defclass beads-agent-backend-agent-shell (beads-agent-backend)
  ((name :initform "agent-shell")
   (priority :initform 15)
   (description :initform "AI agent via shell integration"))
  :documentation "Backend for agent-shell integration.
Uses xenodium's agent-shell package for AI agent sessions.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-agent-shell))
  "Check if agent-shell is available.
Verifies the package is loaded and key functions exist."
  (and ;; Check agent-shell package
       (or (featurep 'agent-shell)
           (require 'agent-shell nil t))
       (fboundp 'agent-shell-start)
       (fboundp 'agent-shell-insert)
       ;; Check agent configs exist
       (boundp 'agent-shell-agent-configs)
       agent-shell-agent-configs))

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-agent-shell) _issue
     system-prompt user-prompt)
  "Start agent-shell session with SYSTEM-PROMPT and USER-PROMPT.
ISSUE is ignored as agent-shell works per-project/workspace.
SYSTEM-PROMPT and USER-PROMPT are combined via
`beads-agent-backend--combine-prompt' (Phase 2 wires the dedicated
system-prompt seam).
The working directory is determined by the caller (may be a worktree).
Returns cons cell (BACKEND-SESSION . BUFFER)."
  ;; Pre-flight checks with helpful error messages
  (unless (or (featurep 'agent-shell) (require 'agent-shell nil t))
    (error "Agent-shell not found.  Install from: https://github.com/xenodium/agent-shell"))
  (require 'agent-shell)
  (unless (and (boundp 'agent-shell-agent-configs) agent-shell-agent-configs)
    (error "No agent-shell configs defined.  Configure `agent-shell-agent-configs'"))
  ;; default-directory is set by beads-agent-start (may be worktree)
  (let* ((prompt (beads-agent-backend--combine-prompt
                  system-prompt user-prompt))
         (config (or beads-agent-agent-shell-config
                     (and (boundp 'agent-shell-preferred-agent-config)
                          agent-shell-preferred-agent-config)
                     (agent-shell-select-config :prompt "Select agent: ")))
         buffer)
    (unless config
      (error "No agent-shell config selected"))
    ;; Start agent-shell session with the config
    (condition-case err
        (progn
          (setq buffer (agent-shell-start :config config))
          (unless buffer
            (error "Agent-shell session started but no buffer returned"))
          ;; Send the initial prompt
          (with-current-buffer buffer
            (agent-shell-insert :text prompt :submit t)))
      (error
       (error "Failed to start agent-shell: %s"
              (error-message-string err))))
    ;; Return (nil . buffer) - agent-shell has no separate session handle
    (cons nil buffer)))

(cl-defmethod beads-agent-backend-stop
    ((backend beads-agent-backend-agent-shell) session)
  "Stop agent-shell SESSION using BACKEND.
Uses standard buffer killing which triggers `agent-shell--clean-up'
via the buffer-local `kill-buffer-hook'.  The cleanup function handles
ACP client shutdown, heartbeat termination, and viewport buffer cleanup.

Uses the stored session buffer for precise cleanup, ensuring only this
session's buffer is killed even when multiple agents share the same
working directory."
  (when-let* ((buf (beads-agent-backend-get-buffer backend session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Optionally interrupt any running request for better UX
        (condition-case nil
            (when (and (get-buffer-process buf)
                       (fboundp 'agent-shell-interrupt))
              (agent-shell-interrupt t))
          (error nil))
        ;; Kill buffer while it's current - agent-shell--clean-up checks
        ;; (derived-mode-p 'agent-shell-mode) which requires current buffer.
        (kill-buffer buf)))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-agent-shell) session)
  "Check if agent-shell SESSION is active.
Returns non-nil if an agent-shell buffer for the session's directory exists
and has a live process."
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-agent-shell--find-buffers working-dir)))
    (and buffers
         (cl-some #'beads-agent-agent-shell--buffer-active-p buffers))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((backend beads-agent-backend-agent-shell) session)
  "Switch to agent-shell buffer for SESSION using BACKEND."
  (if-let* ((buffer (beads-agent-backend-get-buffer backend session)))
      (if (buffer-live-p buffer)
          (beads-agent--pop-to-buffer-other-window buffer)
        (user-error "Agent buffer has been killed"))
    (user-error "No buffer found for session %s" (oref session id))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-agent-shell) session prompt)
  "Send PROMPT to agent-shell for SESSION."
  (require 'agent-shell)
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-agent-shell--find-buffers working-dir))
         (active-buf (cl-find-if #'beads-agent-agent-shell--buffer-active-p
                                 buffers)))
    (if active-buf
        (with-current-buffer active-buf
          (agent-shell-insert :text prompt :submit t))
      (error "No active agent-shell session for %s" working-dir))))

;; Note: beads-agent-backend-get-buffer uses the default implementation
;; from beads-agent-backend.el which returns the stored session buffer.

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-agent-shell))

;;; shell-maker compatibility
;;
;; beads renames agent-shell buffers to its own convention (see
;; `beads-agent--rename-and-store-buffer').  shell-maker identifies its shell
;; buffer by re-deriving the name from the config (`name "agent"' ->
;; `*agent*') and, in the async ACP `session/new' callback, calls
;; `shell-maker-finish-output' from a buffer where the buffer-local
;; `shell-maker--config' is unset.  Together these break shell-maker:
;;
;;   Error running timer: (wrong-type-argument shell-maker-config nil)
;;   Error running timer: (wrong-type-argument processp nil)
;;
;; These advices make shell-maker tolerate both situations.  They are
;; registered here (rather than in shell-maker) because only the renaming
;; performed by beads triggers them.

(defun beads-agent-agent-shell--finish-output-advice (orig &rest args)
  "Bind `shell-maker--config' when ORIG is called with a valid :config.
Allows `shell-maker-finish-output' to be called from a buffer other than
its shell buffer, as agent-shell's async session callback does."
  (let ((config (plist-get args :config)))
    (if (not (and config (fboundp 'shell-maker-buffer)))
        (apply orig args)
      (let* ((shell-buffer (shell-maker-buffer config))
             (shell-maker--config config))
        (with-current-buffer shell-buffer
          (unless (and (boundp 'shell-maker--config) shell-maker--config)
            (setq-local shell-maker--config (copy-sequence config))))
        (apply orig args)))))

(defun beads-agent-agent-shell--buffer-advice (orig config)
  "Return the real shell buffer for CONFIG, honouring external renames.
Falls back to the buffer of the live process named after CONFIG when the
name-derived buffer has no process (which happens after beads renames it)."
  (let ((derived (funcall orig config)))
    (if (get-buffer-process derived)
        derived
      (let ((proc (get-process (shell-maker-process-name config))))
        (or (and proc (process-buffer proc))
            derived)))))

(defun beads-agent-agent-shell--install-shell-maker-advice ()
  "Install the shell-maker compatibility advices."
  (advice-add 'shell-maker-finish-output :around
              #'beads-agent-agent-shell--finish-output-advice
              '((name . beads-agent-agent-shell-finish-output)))
  (advice-add 'shell-maker-buffer :around
              #'beads-agent-agent-shell--buffer-advice
              '((name . beads-agent-agent-shell-buffer))))

(with-eval-after-load 'shell-maker
  (beads-agent-agent-shell--install-shell-maker-advice))


(provide 'beads-agent-agent-shell)
;;; beads-agent-agent-shell.el ends here
