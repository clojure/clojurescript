(ns cljs.nodejscli
  (:require [cljs.nodejs :as nodejs]))

; Call the user's main function
(apply cljs.core/*main-cli-fn* (.argv nodejs/process))

