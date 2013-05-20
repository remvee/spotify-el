;;; spotify.el --- Control the spotify application from emacs

;; Copyright (C) 2012-2013 R.W van 't Veer
;; Copyright (C) 2013 Bjarte Johansen

;; Author: R.W. van 't Veer
;; Created: 18 Oct 2012
;; Keywords: convenience
;; Version: 0.2
;; URL: https://github.com/remvee/spotify-el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Play, pause, skip songs in the Spotify app from Emacs.
;;
;; (global-set-key (kbd "s-<pause>") #'spotify-playpause)
;; (global-set-key (kbd "s-M-<pause>") #'spotify-next)
;;
;; On a system supporting freedesktop.org's D-Bus you can enable song
;; notifications in the minibuffer.
;;
;; (spotify-enable-song-notifications)

;;; Code:

(cond ((fboundp 'dbus-init-bus) (require 'dbus))
      ((string= "darwin" system-type))
      (t (error "Platform not supported")))

(defmacro spotify-eval-only-dbus (&rest body)
  "Only `eval' BODY when D-Bus available."
  (when (fboundp 'dbus-init-bus)
    (eval `(quote (progn ,@body)))))

(defmacro spotify-eval-except-dbus (&rest body)
  "Only `eval' BODY when D-Bus not available."
  (unless (fboundp 'dbus-init-bus)
    (eval `(quote (progn ,@body)))))

(spotify-eval-only-dbus
  (defun spotify-dbus-call (interface method)
    "On INTERFACE call METHOD via D-Bus on the Spotify service."
    (dbus-call-method-asynchronously :session
                                     "org.mpris.MediaPlayer2.spotify"
                                     "/org/mpris/MediaPlayer2"
                                     interface
                                     method
                                     nil))

  (defun spotify-quit ()
    "Quit the spotify application."
    (interactive)
    (spotify-dbus-call "org.mpris.MediaPlayer2" "Quit"))

  (defun spotify-dbus-get-property (interface property)
    "On INTERFACE get value of PROPERTY via D-Bus on the Spotify service."
    (dbus-get-property :session
                       "org.mpris.MediaPlayer2.spotify"
                       "/org/mpris/MediaPlayer2"
                       interface
                       property))

  (defun spotify-humanize-metadata (metadata)
    "Transform METADATA from spotify to a human readable version."
    (when metadata
      (let ((artists (mapconcat 'identity
                                (caadr (assoc "xesam:artist" metadata))
                                ", "))
            (album (caadr (assoc "xesam:album" metadata)))
            (track-nr (caadr (assoc "xesam:trackNumber" metadata)))
            (title (caadr (assoc "xesam:title" metadata))))
        (format "%s / %s / %d: %s" artists album track-nr title))))

  (defun spotify-current ()
    "Return the current song playing in spotify application."
    (interactive)
    (let ((metadata (spotify-dbus-get-property "org.mpris.MediaPlayer2.Player"
                                               "Metadata")))
      (spotify-humanize-metadata metadata)))

  (defun spotify-properties-changed (interface properties &rest ignored)
    "Echo spotify playback status and/or metadata to the mini buffer.

The INTERFACE argument is ignored, PROPERTIES is expected to be
an alist and the IGNORED argument is also ignored."
    (let ((status (caadr (assoc "PlaybackStatus" properties)))
          (current (spotify-humanize-metadata (caadr (assoc "Metadata" properties)))))
      (cond (current (message "Now playing: %s" current))
            (status (message "Spotify %s" status)))))

  (defvar spotify-metadata-change-listener-id nil
    "Object returned by `dbus-register-signal'.")

  (defun spotify-enable-song-notifications ()
    "Enable notifications for the currently playing song in spotify application.

Changes to the currently playing song in spotify will be echoed
to the mini buffer."
    (interactive)
    (setq spotify-metadata-change-listener-id
          (dbus-register-signal :session
                                "org.mpris.MediaPlayer2.Player"
                                "/org/mpris/MediaPlayer2"
                                "org.freedesktop.DBus.Properties"
                                "PropertiesChanged"
                                #'spotify-properties-changed)))

  (defun spotify-disable-song-notifications ()
    "Disable notifications for the currently playing song in spotify application."
    (interactive)
    (dbus-unregister-object spotify-metadata-change-listener-id)))

(spotify-eval-except-dbus
 (defmacro spotify-osa-call (method)
   "Tel Spotify to do METHOD via osascript."
   `(shell-command
     ,(format "osascript -e \"tell application \\\"Spotify\\\" to %s\""
              (cond ((string= "next" (downcase method))
                     "next track")
                    ((string= "previous" (downcase method))
                     "previous track")
                    (t method))))))

(defmacro spotify-defun-player-command (command)
  `(defun ,(intern (concat "spotify-" (downcase command))) ()
     ,(format "Call %s on spotify player." command)
     (interactive)
     ,(if (fboundp 'dbus-init-bus)
          `(spotify-dbus-call "org.mpris.MediaPlayer2.Player" ,command)
        `(spotify-osa-call ,command))
     (message "Spotify %s" ,command)))

;;;###autoload (autoload 'spotify-play "spotify" "Call Play on spotify player." t)
(spotify-defun-player-command "Play")

;;;###autoload (autoload 'spotify-pause "spotify" "Call Pause on spotify player." t)
(spotify-defun-player-command "Pause")

;;;###autoload (autoload 'spotify-playpause "spotify" "Call PlayPause on spotify player." t)
(spotify-defun-player-command "PlayPause")

;;;###autoload (autoload 'spotify-next "spotify" "Call Next on spotify player." t)
(spotify-defun-player-command "Next")

;;;###autoload (autoload 'spotify-previous "spotify" "Call Previous on spotify player." t)
(spotify-defun-player-command "Previous")

;;;###autoload (autoload 'spotify-quit "spotify" "Quit the spotify application." t)
;;;###autoload (autoload 'spotify-enable-song-notifications "spotify" "Enable notifications for the currently playing song in spotify application." t)
;;;###autoload (autoload 'spotify-disable-song-notifications "spotify" "Disable notifications for the currently playing song in spotify application." t)

(spotify-eval-except-dbus
 (spotify-defun-player-command "Quit")

 (defun spotify-enable-song-notifications ()
   "Not implemented for this platform."
   (interactive)
   (error "Not implemented for this platform"))
 (defun spotify-disable-song-notifications ()
   "Not implemented for this platform."
   (interactive)
   (error "Not implemented for this platform")))

(provide 'spotify)

;;; spotify.el ends here
