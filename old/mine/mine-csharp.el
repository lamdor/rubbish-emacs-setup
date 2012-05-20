(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# Code" t)

(add-to-list 'auto-mode-alist '("\\.cs$\\'" . csharp-mode))

(defvar cc-imenu-csharp-generic-expression
  `((nil
     ,(concat
       "[" c-alpha "_][\]\[." c-alnum "_]+[ \t\n\r]+" ; type spec
       "\\([" c-alpha "_][" c-alnum "_]+\\)" ; method name
       "[ \t\n\r]*"
       ;; An argument list that is either empty or contains at least
       ;; two identifiers with only space between them.  This avoids
       ;; matching e.g. "else if (foo)".
       (concat "([ \t\n\r]*"
               "\\([\]\[.," c-alnum "_]+"
               "[ \t\n\r]+"
               "[\]\[.," c-alnum "_]"
               "[\]\[.," c-alnum "_ \t\n\r]*"
               "\\)?)")
       "[.," c-alnum "_ \t\n\r]*"
       "{"
       ) 1))
  "Imenu generic expression for CSharp mode.  See `imenu-generic-expression'.")

(add-hook 'csharp-mode-hook '(lambda ()
                               (cc-imenu-init cc-imenu-csharp-generic-expression)))

(provide 'mine-csharp)