(setenv "EDITOR" "emacsclient -c")
(setenv "JAVA_OPTS" "-Xms256m -Xmx512m -XX:MaxPermSize=256m -d32 -client")
(setenv "MAVEN_OPTS" "-Xmx1024m -Xms256m -XX:MaxPermSize=512m -d32 -client")
(setenv "ANDROID_SDK_HOME" "/usr/local/Cellar/android-sdk/r5")
(setenv "ANDROID_SDK_ROOT" "/usr/local/Cellar/android-sdk/r5")

(provide 'mine-env)