(setq dotemacs-font (font-spec :family "BlexMono Nerd Font" :size 16))
(setq dotemacs-cn-font (font-spec :family "STXihei" :size 16))

(setq dotemacs-package-archives 'custom)
(setq dotemacs-custom-package-archives
      '(("melpa" . "D:/github/elpa-mirror/melpa/")
        ("gnu"   . "D:/github/elpa-mirror/gnu/")))

(setq dotemacs-tempel-path '("/path/to/templates"))

(setq dotemacs-org-html-head
        "<link rel=\"shortcut icon\" href=\"images/rose-red.png\" type=\"image/x-icon\" />
         <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\">"
      dotemacs-org-html-preamble-format
        '(("en" "<header>
                  <nav>
                    <a href=\"/index.html\" class=\"current\">Home</a>
                    <a href=\"/posts/index.html\">Posts</a>
                    <a href=\"/about.html\">About</a>
                  </nav>
                  <h1>%t</h1>
                </header>"))
      dotemacs-org-html-postamble-format
        '(("en" "<footer>
                  <p>
                    &copy xuchengpeng <a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> 29.x  (<a href=\"https://orgmode.org\">Org</a> mode 9.x)
                  </p>
                </footer>")))
