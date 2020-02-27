# Markdown Code block Executor

Run Markdown code block and insert the result, inspred by Org Mode.

For example, let's run a shell code block, <kbd>M-x mdcx</kbd>:


    ```shell
    date
    date -u
    ```
    
    ```result
    Thu Feb 27 09:58:14 CST 2020
    Thu Feb 27 01:58:14 UTC 2020
    ```

and like Org Mode's <kbd>C-c C-c</kbd>, if you rerun the code, the result will
update in-place.

## Support new language

To support language xxx, define a function called `mdcx-run-xxx`, the function
is called with the source code (a string) and must return the result as a
string. For example, to support racket, define `mdcx-run-racket`:

```emacs-lisp
(defun mdcx-run-racket (src)
  (shell-command-to-string
   (concat "racket " (make-temp-file nil nil ".rkt" src))))
```

Type <kbd>C-h f mdcx-run-</kbd> then <kbd>TAB</kbd> to check all supported
languages, currently only shell is supported out of the box.
