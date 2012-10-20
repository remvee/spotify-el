;;; spotify.el --- Control the spotify application from emacs via D-Bus

;; Copyright (C) 2012 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Created: 18 Oct 2012
;; Keywords: convenience
;; Version: 0.1
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

;;; Code:

(require 'dbus)

(defun spotify-dbus-call (interface method)
  "Call METHOD on INTERFACE via D-Bus on the Spotify service."
  (dbus-call-method-asynchronously :session
                                   "org.mpris.MediaPlayer2.spotify"
                                   "/org/mpris/MediaPlayer2"
                                   interface
                                   method
                                   nil))

(defun spotify-dbus-get-property (interface property)
  "Get value of PROPERTY on INTERFACE via D-Bus on the Spotify service."
  (dbus-get-property :session
                     "org.mpris.MediaPlayer2.spotify"
                     "/org/mpris/MediaPlayer2"
                     interface
                     property))

(defun spotify-play ()
  "Start spotify playback."
  (interactive)
  (spotify-dbus-call "org.mpris.MediaPlayer2.Player" "Play"))

(defun spotify-pause ()
  "Pause spotify playback."
  (interactive)
  (spotify-dbus-call "org.mpris.MediaPlayer2.Player" "Pause"))

(defun spotify-playpause ()
  "Play / pause spotify playback."
  (interactive)
  (spotify-dbus-call "org.mpris.MediaPlayer2.Player" "PlayPause"))

(defun spotify-next ()
  "Next song in spotify."
  (interactive)
  (spotify-dbus-call "org.mpris.MediaPlayer2.Player" "Next"))

(defun spotify-previous ()
  "Previous song in spotify."
  (interactive)
  (spotify-dbus-call "org.mpris.MediaPlayer2.Player" "Previous"))

(defun spotify-quit ()
  "Quit the spotify application."
  (interactive)
  (spotify-dbus-call "org.mpris.MediaPlayer2" "Quit"))

(defun spotify-humanize-metadata (metadata)
  "Transform METADATA from spotify to a human readable version."
  (when metadata
    (flet ((join (vals sep) (mapconcat 'identity vals sep)))
      (let ((artists (join (caadr (assoc "xesam:artist" metadata)) ", "))
            (album (caadr (assoc "xesam:album" metadata)))
            (track-nr (caadr (assoc "xesam:trackNumber" metadata)))
            (title (caadr (assoc "xesam:title" metadata))))
        (format "%s / %s / %d: %s" artists album track-nr title)))))

(defun spotify-current ()
  "Return the current song playing in spotify application."
  (interactive)
  (let ((metadata (spotify-dbus-get-property "org.mpris.MediaPlayer2.Player"
                                             "Metadata")))
    (spotify-humanize-metadata metadata)))

(defun spotify-properties-changed (interface properties &rest ignored)
  "Signal listener to echo current song playing in spotify
application to the mini buffer."
  (let ((current (spotify-humanize-metadata (caadr (assoc "Metadata" properties)))))
    (when current
      (message "Now playing: %s" current))))

(defvar spotify-metadata-change-listener-id nil
  "Object returned by `dbus-register-signal' to disable with
`dbus-unregister-object'")

(defun spotify-enable-song-notifications ()
  "Enable notifications for the currently playing song by
spotify application in the minibuffer."
  (interactive)
  (setq spotify-metadata-change-listener-id
        (dbus-register-signal :session
                              "org.mpris.MediaPlayer2.spotify"
                              "/org/mpris/MediaPlayer2"
                              "org.freedesktop.DBus.Properties"
                              "PropertiesChanged"
                              #'spotify-properties-changed)))

(defun spotify-disable-song-notifications ()
  "Disable notifications for the currently playing song by
spotify application in the minibuffer."
  (interactive)
  (dbus-unregister-object spotify-metadata-change-listener-id))

;;; spotify.el ends here
