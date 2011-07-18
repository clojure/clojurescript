; Projects compiled with :target :nodejs have this file appended.  Its
; job is to make sure cljs.nodejs is loaded and that the *main-cli-fn*
; is called with the script's command-line arguments.
(ns cljs.nodejscli
  (:require [cljs.nodejs :as nodejs]))

; Call the user's main function
(apply cljs.core/*main-cli-fn* (drop 2 (.argv nodejs/process)))

