(setenv "EDITOR" "emacsclient -c")
(setenv "JAVA_OPTS" "-Xms256m -Xmx512m -XX:MaxPermSize=256m -client")
(setenv "MAVEN_OPTS" "-Xmx1024m -Xms256m -XX:MaxPermSize=512m -client")
(setenv "ANDROID_SDK_HOME" "/usr/local/Cellar/android-sdk/r5")
(setenv "ANDROID_SDK_ROOT" "/usr/local/Cellar/android-sdk/r5")

(setenv "CLOJURESCRIPT_HOME" (concat (getenv "HOME") "/code/watch/clojurescript"))

(provide 'mine-env)