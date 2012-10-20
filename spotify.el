;;; spotify.el --- Control the spotify application from emacs via dbus

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

(defconst spotify-dbus-send-base
  "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.")

(defun spotify-dbus-send (command)
  "Send dbus COMMAND to spotify."
  (shell-command (concat spotify-dbus-send-base command)))

(defun spotify-play ()
  "Start spotify playback."
  (interactive)
  (spotify-dbus-send "Player.Play"))

(defun spotify-pause ()
  "Pause spotify playback."
  (interactive)
  (spotify-dbus-send "Player.Pause"))

(defun spotify-playpause ()
  "Play / pause spotify playback."
  (interactive)
  (spotify-dbus-send "Player.PlayPause"))

(defun spotify-next ()
  "Next song in spotify."
  (interactive)
  (spotify-dbus-send "Player.Next"))

(defun spotify-previous ()
  "Previous song in spotify."
  (interactive)
  (spotify-dbus-send "Player.Previous"))

(defun spotify-quit ()
  "Quit the spotify application."
  (interactive)
  (spotify-dbus-send "Quit"))

;;; spotify.el ends here
