(setq dotemacs-font (font-spec :family "BlexMono Nerd Font" :size 17))
(setq dotemacs-cn-font (font-spec :family "Microsoft Yahei" :size 16))

(setq dotemacs-package-archives 'custom)
(setq package-archives
      '(("gnu"   . "d:/github/elpa-mirror/gnu/")
        ("melpa" . "d:/github/elpa-mirror/melpa/")))

(setq dotemacs-tempel-path '("/path/to/templates"))

(setq dotemacs-org-site-dir "d:/github/org-site/")

(setq dotemacs-org-html-head
        "<link rel=\"shortcut icon\" href=\"/images/rose-red.png\" type=\"image/x-icon\">
         <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\">
         <script src=\"/js/copycode.js\"></script>"
      dotemacs-org-html-preamble-format
        '(("en" "<a href=\"/index.html\" style=\"text-decoration:underline\">Home</a>
                 <a href=\"/posts/index.html\" style=\"text-decoration:underline\">Posts</a>
                 <a href=\"/about.html\" style=\"text-decoration:underline\">About</a>"))
      dotemacs-org-html-postamble-format
        '(("en" "<div class=\"nav-btn\"><a href=\"/index.html\">Home</a></div>
                 <div class=\"top-btn\"><a href=\"#top\">Top</a></div>
                 <footer>
                  <p>&copy xuchengpeng <a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> 29.x  (<a href=\"https://orgmode.org\">Org</a> mode 9.x)</p>
                 </footer>")))
