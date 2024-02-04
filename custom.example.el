(setq user-full-name "Chuck"
      user-mail-address "330476629@qq.com")

(setq dotemacs-font (font-spec :family "BlexMono Nerd Font" :size 17)
      dotemacs-cn-font (font-spec :family "Microsoft Yahei"))

(setq dotemacs-package-archives 'custom)
(setq package-archives
      '(("gnu"   . "d:/github/elpa-mirror/gnu/")
        ("melpa" . "d:/github/elpa-mirror/melpa/")))

(setq dotemacs-tempel-path '("d:/github/dotfiles/tempel/templates"))

(setq dotemacs-org-site-dir "d:/github/org-site/"
      dotemacs-org-html-head
      "<link rel=\"shortcut icon\" href=\"/images/rose-red.png\" type=\"image/x-icon\">
       <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\">
       <script src=\"/js/copycode.js\"></script>"
      dotemacs-org-html-preamble-format
      '(("en" "<div class=\"nav-bar\">
               <a class=\"nav-link\" href=\"/index.html\">Home</a>
               <a class=\"nav-link\" href=\"/posts/index.html\">Posts</a>
               <a class=\"nav-link\" href=\"/about.html\">About</a>
               </div>"))
      dotemacs-org-html-postamble-format
      '(("en" "<div class=\"nav-btn\"><a href=\"/index.html\">Home</a></div>
               <div class=\"top-btn\"><a href=\"#top\">Top</a></div>
               <footer>
                <p>&copy xuchengpeng <a href=\"http://www.gnu.org/software/emacs/\">Emacs</a> 29.2  (<a href=\"https://orgmode.org\">Org</a> mode 9.6.15)</p>
               </footer>")))
