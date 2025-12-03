;;; ridiculous-coding.el --- Over-the-top coding effects -*- lexical-binding: t -*-
;;; Commentary:
;; Port of https://github.com/jotson/ridiculous_coding (Godot plugin)
;; Makes your coding experience absurdly dramatic with:
;; - Explosions and particles on typing
;; - Screen shake
;; - Sound effects (cross-platform)
;; - Combo counters
;; - Different effects for different actions
;;
;; Toggle with M-x ridiculous-coding-mode
;; Go full chaos with M-x global-ridiculous-coding-mode
;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup ridiculous-coding nil
  "Over-the-top visual nonsense while you type."
  :group 'convenience
  :prefix "ridiculous-coding-")

(defcustom ridiculous-coding-intensity 0.25
  "Base probability (0-1) that an effect fires on each keypress.
Higher values = more chaos."
  :type 'float)

(defcustom ridiculous-coding-sound-enabled t
  "Whether to play sound effects."
  :type 'boolean)

(defcustom ridiculous-coding-sound-volume 0.5
  "Volume for sound effects (0.0-1.0). Only works on some platforms."
  :type 'float)

(defcustom ridiculous-coding-sounds-directory
  (expand-file-name "sounds/ridiculous" user-emacs-directory)
  "Directory containing sound effect files.
Expected subdirs: typing/, delete/, save/, combo/"
  :type 'directory)

(defcustom ridiculous-coding-images-directory
  (expand-file-name "images/ridiculous" user-emacs-directory)
  "Directory containing image sprite sheets.
Expected files: blip.png, boom.png, newline.png"
  :type 'directory)

(defcustom ridiculous-coding-images-enabled t
  "Whether to use image animations (GUI mode only).
Falls back to text effects in terminal."
  :type 'boolean)

(defcustom ridiculous-coding-shake-enabled t
  "Whether to enable screen shake effects."
  :type 'boolean)

(defcustom ridiculous-coding-particles-enabled t
  "Whether to enable particle/explosion effects."
  :type 'boolean)

(defcustom ridiculous-coding-combo-enabled t
  "Whether to track and display combo counter."
  :type 'boolean)

(defcustom ridiculous-coding-combo-timeout 2.0
  "Seconds of inactivity before combo resets."
  :type 'float)

(defcustom ridiculous-coding-combo-threshold 10
  "Combo count needed to trigger bonus effects."
  :type 'integer)

(defcustom ridiculous-coding-rainbow-enabled t
  "Whether to enable rainbow trail on typed characters."
  :type 'boolean)

(defcustom ridiculous-coding-spirits-enabled t
  "Whether characters float upward like escaping souls."
  :type 'boolean)

(defcustom ridiculous-coding-shockwave-enabled t
  "Whether to show expanding shockwave rings."
  :type 'boolean)

(defcustom ridiculous-coding-flash-enabled t
  "Whether to flash the screen on big combos."
  :type 'boolean)

(defcustom ridiculous-coding-afterimage-enabled t
  "Whether typed characters leave ghostly afterimages."
  :type 'boolean)

;;; ============================================================================
;;; Internal State
;;; ============================================================================

(defvar-local ridiculous-coding--overlays nil
  "Active overlays for effects.")

(defvar-local ridiculous-coding--combo-count 0
  "Current combo counter.")

(defvar-local ridiculous-coding--combo-timer nil
  "Timer for combo timeout.")

(defvar-local ridiculous-coding--last-action nil
  "Last action type: typing, delete, save.")

(defvar ridiculous-coding--sound-process nil
  "Current sound process (to avoid overlap).")

;;; ============================================================================
;;; Platform Detection & Sound
;;; ============================================================================

(defun ridiculous-coding--platform ()
  "Return platform identifier: macos, linux, or other."
  (cond
   ((eq system-type 'darwin) 'macos)
   ((eq system-type 'gnu/linux) 'linux)
   (t 'other)))

(defun ridiculous-coding--sound-command ()
  "Return the sound playback command for current platform."
  (pcase (ridiculous-coding--platform)
    ('macos "afplay")
    ('linux (or (executable-find "paplay")
                (executable-find "aplay")
                (executable-find "play")))  ; sox
    (_ nil)))

(defun ridiculous-coding--play-sound (category)
  "Play a random sound from CATEGORY (typing, delete, save, combo).
Sounds are loaded from `ridiculous-coding-sounds-directory'/CATEGORY/."
  (when (and ridiculous-coding-sound-enabled
             (ridiculous-coding--sound-command))
    (let* ((dir (expand-file-name (symbol-name category)
                                  ridiculous-coding-sounds-directory))
           (files (and (file-directory-p dir)
                       (directory-files dir t "\\.\\(wav\\|mp3\\|ogg\\|aiff\\)$")))
           (sound (and files (nth (random (length files)) files)))
           (cmd (ridiculous-coding--sound-command)))
      (when sound
        ;; Kill previous sound to avoid cacophony
        (when (and ridiculous-coding--sound-process
                   (process-live-p ridiculous-coding--sound-process))
          (delete-process ridiculous-coding--sound-process))
        (setq ridiculous-coding--sound-process
              (pcase (ridiculous-coding--platform)
                ('macos
                 (start-process "ridiculous-sound" nil cmd
                                "-v" (format "%.1f" ridiculous-coding-sound-volume)
                                sound))
                ('linux
                 ;; paplay doesn't have easy volume, aplay has no volume
                 ;; Could use pactl or sox for volume control
                 (start-process "ridiculous-sound" nil cmd sound))
                (_ nil)))))))

;;; ============================================================================
;;; Visual Effects: Particles & Explosions
;;; ============================================================================

(defconst ridiculous-coding--explosion-chars
  '("*" "+" "x" "X" "o" "O" "." ":" "!" "#" "@" "$" "%" "&")
  "Characters used for explosion particles.")

(defconst ridiculous-coding--explosion-faces
  '((:foreground "#FF6B6B" :weight bold)
    (:foreground "#FFE66D" :weight bold)
    (:foreground "#4ECDC4" :weight bold)
    (:foreground "#FF8C42" :weight bold)
    (:foreground "#A855F7" :weight bold)
    (:foreground "#22D3EE" :weight bold))
  "Faces for explosion particles (colorful!).")

(defun ridiculous-coding--random-element (list)
  "Return random element from LIST."
  (nth (random (length list)) list))

(defun ridiculous-coding--spawn-particle (pos char face duration)
  "Spawn a particle overlay at POS with CHAR, FACE, lasting DURATION seconds."
  (when (and (>= pos (point-min)) (<= pos (point-max)))
    (let ((ov (make-overlay pos (min (1+ pos) (point-max)))))
      (overlay-put ov 'priority 9999)
      (overlay-put ov 'ridiculous t)
      (overlay-put ov 'display (propertize char 'face face))
      (push ov ridiculous-coding--overlays)
      (run-at-time duration nil #'ridiculous-coding--remove-overlay ov))))

(defun ridiculous-coding--remove-overlay (ov)
  "Remove overlay OV and clean up."
  (when (overlayp ov)
    (delete-overlay ov)
    (setq ridiculous-coding--overlays
          (delq ov ridiculous-coding--overlays))))

(defun ridiculous-coding--cleanup-overlays ()
  "Remove all ridiculous overlays."
  (mapc #'delete-overlay ridiculous-coding--overlays)
  (setq ridiculous-coding--overlays nil))

(defun ridiculous-coding--explosion-at-point ()
  "Create an explosion effect at point."
  (when ridiculous-coding-particles-enabled
    (let* ((pos (point))
           (num-particles (+ 3 (random 5))))
      (dotimes (_ num-particles)
        (let* ((offset (- (random 7) 3))  ; -3 to +3
               (particle-pos (+ pos offset))
               (char (ridiculous-coding--random-element
                      ridiculous-coding--explosion-chars))
               (face (ridiculous-coding--random-element
                      ridiculous-coding--explosion-faces))
               (duration (+ 0.05 (* (random 10) 0.01))))  ; 0.05-0.15s
          (ridiculous-coding--spawn-particle particle-pos char face duration))))))

(defun ridiculous-coding--big-explosion ()
  "Create a BIG explosion (for combos, saves, etc.)."
  (when ridiculous-coding-particles-enabled
    (let* ((pos (point))
           (num-particles (+ 8 (random 8))))
      (dotimes (_ num-particles)
        (let* ((offset (- (random 15) 7))  ; wider spread
               (particle-pos (+ pos offset))
               (char (ridiculous-coding--random-element
                      ridiculous-coding--explosion-chars))
               (face (ridiculous-coding--random-element
                      ridiculous-coding--explosion-faces))
               (duration (+ 0.1 (* (random 15) 0.01))))
          (ridiculous-coding--spawn-particle particle-pos char face duration))))))

;;; ============================================================================
;;; Visual Effects: Screen Shake
;;; ============================================================================

(defun ridiculous-coding--shake (intensity)
  "Shake the screen with given INTENSITY (1-3)."
  (when ridiculous-coding-shake-enabled
    (let* ((win (selected-window))
           (orig-margins (window-margins win))
           (left (or (car orig-margins) 0))
           (right (or (cdr orig-margins) 0))
           (dx (* intensity 1)))
      (cl-flet ((set-m (delta)
                  (set-window-margins win (max 0 (+ left delta)) right)))
        ;; Quick jiggle sequence
        (set-m dx)
        (run-at-time 0.02 nil (lambda () (set-m (- dx))))
        (run-at-time 0.04 nil (lambda () (set-m dx)))
        (run-at-time 0.06 nil (lambda () (set-m 0)))))))

(defun ridiculous-coding--small-shake ()
  "Small screen shake for regular typing."
  (ridiculous-coding--shake 1))

(defun ridiculous-coding--big-shake ()
  "Big screen shake for deletes, saves, combos."
  (ridiculous-coding--shake 2))

;;; ============================================================================
;;; Visual Effects: INSANE MODE
;;; ============================================================================

;; Rainbow color cycle
(defconst ridiculous-coding--rainbow-colors
  '("#FF0000" "#FF7F00" "#FFFF00" "#00FF00" "#0000FF" "#4B0082" "#9400D3")
  "Rainbow colors for trail effect.")

(defvar-local ridiculous-coding--rainbow-index 0
  "Current position in rainbow cycle.")

(defun ridiculous-coding--next-rainbow-color ()
  "Get next color in rainbow cycle."
  (let ((color (nth ridiculous-coding--rainbow-index
                    ridiculous-coding--rainbow-colors)))
    (setq ridiculous-coding--rainbow-index
          (mod (1+ ridiculous-coding--rainbow-index)
               (length ridiculous-coding--rainbow-colors)))
    color))

(defun ridiculous-coding--rainbow-trail ()
  "Apply rainbow color to the just-typed character."
  (when (and ridiculous-coding-rainbow-enabled
             (> (point) (point-min)))
    (let* ((pos (1- (point)))
           (color (ridiculous-coding--next-rainbow-color))
           (ov (make-overlay pos (1+ pos))))
      (overlay-put ov 'priority 100)
      (overlay-put ov 'ridiculous t)
      (overlay-put ov 'face `(:foreground ,color :weight bold))
      (push ov ridiculous-coding--overlays)
      ;; Fade out over time with multiple steps
      (run-at-time 0.3 nil
                   (lambda (o)
                     (when (overlayp o)
                       (overlay-put o 'face `(:foreground ,color :weight normal))))
                   ov)
      (run-at-time 0.6 nil #'ridiculous-coding--remove-overlay ov))))

;; Floating spirits - characters drift upward
(defun ridiculous-coding--spawn-spirit (char)
  "Spawn CHAR as a spirit that floats upward."
  (when ridiculous-coding-spirits-enabled
    (let* ((start-pos (point))
           (col (current-column))
           (line (line-number-at-pos))
           (spirit-char (propertize char 'face
                                    `(:foreground "#88FFFF" :weight bold :height 1.2)))
           (frames 5)
           (frame-delay 0.06))
      ;; Animate upward by creating overlays on lines above
      (dotimes (i frames)
        (run-at-time (* i frame-delay) nil
                     (lambda (idx ch)
                       (save-excursion
                         (goto-char (point-min))
                         (forward-line (max 0 (- line idx 2)))
                         (move-to-column col t)
                         (let ((ov (make-overlay (point) (1+ (point)))))
                           (overlay-put ov 'priority 10000)
                           (overlay-put ov 'ridiculous t)
                           ;; Fade as it rises
                           (let* ((alpha (- 1.0 (/ (float idx) frames)))
                                  (gray (floor (* alpha 255))))
                             (overlay-put ov 'display
                                          (propertize ch 'face
                                                      `(:foreground
                                                        ,(format "#%02X%02X%02X"
                                                                 (floor (* alpha 136))
                                                                 (floor (* alpha 255))
                                                                 (floor (* alpha 255)))
                                                        :height ,(+ 1.0 (* 0.1 idx))))))
                           (push ov ridiculous-coding--overlays)
                           (run-at-time frame-delay nil
                                        #'ridiculous-coding--remove-overlay ov))))
                     i spirit-char)))))

;; Shockwave rings expanding outward
(defun ridiculous-coding--shockwave ()
  "Create expanding ring shockwave from point."
  (when ridiculous-coding-shockwave-enabled
    (let* ((center (point))
           (frames '(("·" . 0.0)
                     ("○" . 0.04)
                     ("◯" . 0.08)
                     ("◎" . 0.12)
                     ("●" . 0.16)))
           (colors '("#FFFFFF" "#FFFF88" "#FFAA44" "#FF4444" "#880000")))
      (cl-loop for (char . delay) in frames
               for color in colors
               for i from 0
               do (run-at-time delay nil
                               (lambda (c col pos)
                                 (when (and (>= pos (point-min))
                                            (< pos (point-max)))
                                   (let ((ov (make-overlay pos (min (1+ pos) (point-max)))))
                                     (overlay-put ov 'priority (- 10000 i))
                                     (overlay-put ov 'ridiculous t)
                                     (overlay-put ov 'display
                                                  (propertize c 'face
                                                              `(:foreground ,col :height 1.5)))
                                     (push ov ridiculous-coding--overlays)
                                     (run-at-time 0.05 nil
                                                  #'ridiculous-coding--remove-overlay ov))))
                               char color center)))))

;; Afterimage ghost effect
(defun ridiculous-coding--afterimage (char)
  "Create a ghostly afterimage of CHAR that fades."
  (when (and ridiculous-coding-afterimage-enabled
             (> (point) (point-min)))
    (let* ((pos (1- (point)))
           (ghost-colors '("#FFFFFF" "#CCCCCC" "#999999" "#666666" "#333333"))
           (delays '(0.0 0.05 0.1 0.15 0.2)))
      (cl-loop for color in ghost-colors
               for delay in delays
               do (run-at-time delay nil
                               (lambda (p c col)
                                 (when (and (>= p (point-min)) (< p (point-max)))
                                   (let ((ov (make-overlay p (1+ p))))
                                     (overlay-put ov 'priority 50)
                                     (overlay-put ov 'ridiculous t)
                                     (overlay-put ov 'after-string
                                                  (propertize c 'face
                                                              `(:foreground ,col)))
                                     (push ov ridiculous-coding--overlays)
                                     (run-at-time 0.05 nil
                                                  #'ridiculous-coding--remove-overlay ov))))
                               pos char color)))))

;; Screen flash - pulse background on combos
(defun ridiculous-coding--flash (color)
  "Flash the screen with COLOR."
  (when ridiculous-coding-flash-enabled
    (let* ((win (selected-window))
           (buf (window-buffer win))
           (orig-bg (face-background 'default))
           (flash-face (make-symbol "flash-face")))
      ;; Create temporary face
      (face-spec-set flash-face `((t :background ,color)))
      ;; Apply flash overlay to whole visible region
      (let ((ov (make-overlay (window-start win) (window-end win))))
        (overlay-put ov 'priority 20000)
        (overlay-put ov 'ridiculous t)
        (overlay-put ov 'face `(:background ,color))
        (push ov ridiculous-coding--overlays)
        ;; Quick flash sequence
        (run-at-time 0.03 nil
                     (lambda (o)
                       (when (overlayp o)
                         (overlay-put o 'face `(:background ,(ridiculous-coding--blend-colors color orig-bg 0.5)))))
                     ov)
        (run-at-time 0.06 nil #'ridiculous-coding--remove-overlay ov)))))

(defun ridiculous-coding--blend-colors (c1 c2 ratio)
  "Blend C1 and C2 by RATIO (0.0 = c1, 1.0 = c2)."
  (let* ((rgb1 (color-name-to-rgb c1))
         (rgb2 (color-name-to-rgb c2)))
    (if (and rgb1 rgb2)
        (format "#%02X%02X%02X"
                (floor (* 255 (+ (* (nth 0 rgb1) (- 1 ratio)) (* (nth 0 rgb2) ratio))))
                (floor (* 255 (+ (* (nth 1 rgb1) (- 1 ratio)) (* (nth 1 rgb2) ratio))))
                (floor (* 255 (+ (* (nth 2 rgb1) (- 1 ratio)) (* (nth 2 rgb2) ratio)))))
      c1)))

;; MEGA combo effects
(defun ridiculous-coding--mega-combo ()
  "Unleash visual chaos for big combo milestones."
  (ridiculous-coding--flash "#FFFF00")
  (ridiculous-coding--shockwave)
  (dotimes (_ 3)
    (ridiculous-coding--spawn-spirit
     (ridiculous-coding--random-element '("★" "✦" "✧" "⚡" "💥" "🔥")))))

;;; ============================================================================
;;; Visual Effects: IMAGE ANIMATIONS (GUI only)
;;; ============================================================================

(defun ridiculous-coding--gui-p ()
  "Return t if running in GUI mode with image support."
  (and (display-graphic-p)
       ridiculous-coding-images-enabled
       (image-type-available-p 'png)))

(defun ridiculous-coding--load-sprite-sheet (filename)
  "Load sprite sheet FILENAME from images directory."
  (let ((path (expand-file-name filename ridiculous-coding-images-directory)))
    (when (file-exists-p path)
      path)))

(defun ridiculous-coding--extract-frame (sprite-path frame-width frame-height frame-index)
  "Extract a frame from sprite sheet at SPRITE-PATH.
FRAME-WIDTH and FRAME-HEIGHT are the dimensions of each frame.
FRAME-INDEX is which frame to extract (0-indexed, left to right)."
  (when sprite-path
    (let ((x-offset (* frame-index frame-width)))
      (create-image sprite-path 'png nil
                    :crop (list x-offset 0 frame-width frame-height)
                    :scale 0.5  ; Scale down for editor
                    :ascent 'center))))

(defun ridiculous-coding--animate-sprite (sprite-path frame-width frame-height
                                                       num-frames frame-delay)
  "Animate sprite sheet at point.
SPRITE-PATH is the image file.
FRAME-WIDTH/HEIGHT are frame dimensions.
NUM-FRAMES is total frame count.
FRAME-DELAY is seconds between frames."
  (when (and (ridiculous-coding--gui-p) sprite-path)
    (let ((pos (point)))
      (dotimes (i num-frames)
        (run-at-time (* i frame-delay) nil
                     (lambda (frame-idx p)
                       (when (and (buffer-live-p (current-buffer))
                                  (>= p (point-min))
                                  (< p (point-max)))
                         (let* ((img (ridiculous-coding--extract-frame
                                      sprite-path frame-width frame-height frame-idx))
                                (ov (make-overlay p (1+ p))))
                           (when img
                             (overlay-put ov 'priority 15000)
                             (overlay-put ov 'ridiculous t)
                             (overlay-put ov 'after-string (propertize " " 'display img))
                             (push ov ridiculous-coding--overlays)
                             (run-at-time frame-delay nil
                                          #'ridiculous-coding--remove-overlay ov)))))
                     i pos)))))

(defun ridiculous-coding--boom-animation ()
  "Play the boom explosion animation at point."
  (if (ridiculous-coding--gui-p)
      (let ((sprite (ridiculous-coding--load-sprite-sheet "boom.png")))
        (when sprite
          ;; boom.png is 768x256, 6 frames of 128x256
          (ridiculous-coding--animate-sprite sprite 128 256 6 0.05)))
    ;; Fallback to text explosion
    (ridiculous-coding--big-explosion)))

(defun ridiculous-coding--blip-animation ()
  "Play the blip animation at point."
  (if (ridiculous-coding--gui-p)
      (let ((sprite (ridiculous-coding--load-sprite-sheet "blip.png")))
        (when sprite
          ;; blip.png is 256x32, 8 frames of 32x32
          (ridiculous-coding--animate-sprite sprite 32 32 8 0.03)))
    ;; Fallback to text explosion
    (ridiculous-coding--explosion-at-point)))

(defun ridiculous-coding--newline-animation ()
  "Play the newline animation."
  (if (ridiculous-coding--gui-p)
      (let ((sprite (ridiculous-coding--load-sprite-sheet "newline.png")))
        (when sprite
          ;; newline.png is 320x64, 5 frames of 64x64
          (ridiculous-coding--animate-sprite sprite 64 64 5 0.04)))
    ;; Fallback
    (ridiculous-coding--shockwave)))

;;; ============================================================================
;;; Combo System
;;; ============================================================================

(defun ridiculous-coding--update-combo ()
  "Increment combo counter and handle combo events."
  (when ridiculous-coding-combo-enabled
    ;; Cancel existing timeout
    (when ridiculous-coding--combo-timer
      (cancel-timer ridiculous-coding--combo-timer))
    ;; Increment
    (cl-incf ridiculous-coding--combo-count)
    ;; Check for combo threshold
    (when (and (> ridiculous-coding--combo-count 0)
               (= (mod ridiculous-coding--combo-count
                       ridiculous-coding-combo-threshold) 0))
      (ridiculous-coding--combo-bonus))
    ;; Set timeout
    (setq ridiculous-coding--combo-timer
          (run-at-time ridiculous-coding-combo-timeout nil
                       #'ridiculous-coding--reset-combo))))

(defun ridiculous-coding--reset-combo ()
  "Reset the combo counter."
  (setq ridiculous-coding--combo-count 0)
  (when ridiculous-coding--combo-timer
    (cancel-timer ridiculous-coding--combo-timer)
    (setq ridiculous-coding--combo-timer nil)))

(defun ridiculous-coding--combo-bonus ()
  "Trigger bonus effects for hitting combo threshold."
  (ridiculous-coding--mega-combo)
  ;; Use image boom if available, otherwise text explosion
  (ridiculous-coding--boom-animation)
  (ridiculous-coding--big-shake)
  (ridiculous-coding--play-sound 'combo)
  (message "🔥 COMBO x%d! 🔥" ridiculous-coding--combo-count))

;;; ============================================================================
;;; Event Handlers
;;; ============================================================================

(defun ridiculous-coding--maybe-p (probability)
  "Return t with given PROBABILITY (0.0-1.0)."
  (< (cl-random 1.0) probability))

(defun ridiculous-coding--on-typing ()
  "Handle typing event."
  (ridiculous-coding--update-combo)
  (let* ((char (char-to-string last-command-event))
         (combo ridiculous-coding--combo-count)
         (intensity (+ ridiculous-coding-intensity
                       (* 0.01 (min combo 20))))
         (is-newline (eq last-command-event ?\n)))
    ;; Special handling for newlines
    (when is-newline
      (ridiculous-coding--newline-animation))
    ;; Rainbow trail - ALWAYS on when enabled (it's the baseline pop)
    (ridiculous-coding--rainbow-trail)
    ;; Afterimage echo on every keystroke for that juicy feel
    (when (ridiculous-coding--maybe-p 0.7)
      (ridiculous-coding--afterimage char))
    ;; Explosions/blips scale with combo
    (when (ridiculous-coding--maybe-p intensity)
      ;; Use image blip in GUI, text explosion in terminal
      (if (ridiculous-coding--gui-p)
          (ridiculous-coding--blip-animation)
        (ridiculous-coding--explosion-at-point))
      (when (ridiculous-coding--maybe-p 0.3)
        (ridiculous-coding--small-shake))
      (when (ridiculous-coding--maybe-p 0.1)
        (ridiculous-coding--play-sound 'typing)))
    ;; Spirits escape occasionally
    (when (and (> combo 5) (ridiculous-coding--maybe-p 0.15))
      (ridiculous-coding--spawn-spirit char))
    ;; Shockwaves at higher combos
    (when (and (> combo 15) (ridiculous-coding--maybe-p 0.1))
      (ridiculous-coding--shockwave))))

(defun ridiculous-coding--on-delete ()
  "Handle deletion event."
  (when (ridiculous-coding--maybe-p 0.4)
    (ridiculous-coding--explosion-at-point)
    (ridiculous-coding--small-shake)
    (when (ridiculous-coding--maybe-p 0.2)
      (ridiculous-coding--play-sound 'delete))))

(defun ridiculous-coding--on-save ()
  "Handle save event - always dramatic."
  (ridiculous-coding--big-explosion)
  (ridiculous-coding--big-shake)
  (ridiculous-coding--play-sound 'save)
  (message "SAVED!"))

;;; ============================================================================
;;; Hooks
;;; ============================================================================

(defun ridiculous-coding--post-self-insert ()
  "Hook for post-self-insert-hook."
  (ridiculous-coding--on-typing))

(defun ridiculous-coding--after-change (beg end len)
  "Hook for after-change-functions. BEG END LEN are change params."
  ;; Deletion happened if len > 0 and no new text
  (when (and (> len 0) (= beg end))
    (ridiculous-coding--on-delete)))

(defun ridiculous-coding--after-save ()
  "Hook for after-save-hook."
  (ridiculous-coding--on-save))

;;; ============================================================================
;;; Minor Mode
;;; ============================================================================

;;;###autoload
(define-minor-mode ridiculous-coding-mode
  "Toggle ridiculous coding effects.
Makes your coding experience absurdly dramatic with explosions,
screen shake, sounds, and combo counters."
  :lighter " BOOM"
  :group 'ridiculous-coding
  (if ridiculous-coding-mode
      (progn
        (add-hook 'post-self-insert-hook
                  #'ridiculous-coding--post-self-insert nil t)
        (add-hook 'after-change-functions
                  #'ridiculous-coding--after-change nil t)
        (add-hook 'after-save-hook
                  #'ridiculous-coding--after-save nil t)
        (add-hook 'kill-buffer-hook
                  #'ridiculous-coding--cleanup-overlays nil t))
    ;; Cleanup
    (remove-hook 'post-self-insert-hook
                 #'ridiculous-coding--post-self-insert t)
    (remove-hook 'after-change-functions
                 #'ridiculous-coding--after-change t)
    (remove-hook 'after-save-hook
                 #'ridiculous-coding--after-save t)
    (remove-hook 'kill-buffer-hook
                 #'ridiculous-coding--cleanup-overlays t)
    (ridiculous-coding--cleanup-overlays)
    (ridiculous-coding--reset-combo)))

;;;###autoload
(define-globalized-minor-mode global-ridiculous-coding-mode
  ridiculous-coding-mode
  (lambda ()
    (when (derived-mode-p 'prog-mode 'text-mode)
      (ridiculous-coding-mode 1))))

;;; ============================================================================
;;; Interactive Commands
;;; ============================================================================

(defun ridiculous-coding-test-explosion ()
  "Test the explosion effect."
  (interactive)
  (ridiculous-coding--big-explosion))

(defun ridiculous-coding-test-shake ()
  "Test the screen shake effect."
  (interactive)
  (ridiculous-coding--big-shake))

(defun ridiculous-coding-test-sound (category)
  "Test playing a sound from CATEGORY."
  (interactive
   (list (intern (completing-read "Category: "
                                  '("typing" "delete" "save" "combo")))))
  (ridiculous-coding--play-sound category))

(defun ridiculous-coding-set-intensity (level)
  "Set intensity to LEVEL (low, medium, high, insane)."
  (interactive
   (list (intern (completing-read "Intensity: "
                                  '("low" "medium" "high" "insane")))))
  (setq ridiculous-coding-intensity
        (pcase level
          ('low 0.05)
          ('medium 0.15)
          ('high 0.3)
          ('insane 0.6)))
  (message "Ridiculous intensity: %s (%.0f%%)" level
           (* 100 ridiculous-coding-intensity)))

(defun ridiculous-coding-test-rainbow ()
  "Test rainbow trail effect."
  (interactive)
  (dotimes (_ 20)
    (ridiculous-coding--rainbow-trail)
    (forward-char 1)))

(defun ridiculous-coding-test-spirit ()
  "Test spirit floating effect."
  (interactive)
  (ridiculous-coding--spawn-spirit "★"))

(defun ridiculous-coding-test-shockwave ()
  "Test shockwave effect."
  (interactive)
  (ridiculous-coding--shockwave))

(defun ridiculous-coding-test-flash ()
  "Test screen flash effect."
  (interactive)
  (ridiculous-coding--flash "#FF4444"))

(defun ridiculous-coding-test-mega ()
  "Test mega combo effect (all the things)."
  (interactive)
  (ridiculous-coding--mega-combo)
  (ridiculous-coding--big-explosion)
  (ridiculous-coding--big-shake)
  (message "🔥 MEGA TEST! 🔥"))

(defun ridiculous-coding-test-all ()
  "Test all effects in sequence."
  (interactive)
  (ridiculous-coding--rainbow-trail)
  (run-at-time 0.3 nil #'ridiculous-coding--explosion-at-point)
  (run-at-time 0.5 nil (lambda () (ridiculous-coding--spawn-spirit "✦")))
  (run-at-time 0.8 nil #'ridiculous-coding--shockwave)
  (run-at-time 1.0 nil (lambda () (ridiculous-coding--flash "#FFFF00")))
  (run-at-time 1.2 nil #'ridiculous-coding--big-shake)
  (message "Testing all effects..."))

(defun ridiculous-coding-test-blip ()
  "Test the blip image animation."
  (interactive)
  (if (ridiculous-coding--gui-p)
      (ridiculous-coding--blip-animation)
    (message "Images only work in GUI mode")))

(defun ridiculous-coding-test-boom ()
  "Test the boom image animation."
  (interactive)
  (if (ridiculous-coding--gui-p)
      (ridiculous-coding--boom-animation)
    (message "Images only work in GUI mode")))

(defun ridiculous-coding-test-newline ()
  "Test the newline image animation."
  (interactive)
  (if (ridiculous-coding--gui-p)
      (ridiculous-coding--newline-animation)
    (message "Images only work in GUI mode")))

(defun ridiculous-coding-toggle-images ()
  "Toggle image animations on/off."
  (interactive)
  (setq ridiculous-coding-images-enabled (not ridiculous-coding-images-enabled))
  (message "Image animations: %s" (if ridiculous-coding-images-enabled "ON" "OFF")))

(provide 'ridiculous-coding)
;;; ridiculous-coding.el ends here
