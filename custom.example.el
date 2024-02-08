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
      '(("en" "<div class=\"header-wrapper\">
               <div class=\"site-header\">
               <a class=\"site-title\" href=\"/\">Chuck</a>
               <div class=\"site-nav\">
                 <a class=\"nav-link\" href=\"/posts/index.html\">Posts</a>
                 <a class=\"nav-link\" href=\"/search.html\">Search</a>
                 <a class=\"nav-link\" href=\"/about.html\">About</a>
               </div>
               </div>
               </div>"))
      dotemacs-org-html-postamble-format
      '(("en" "<div class=\"nav-btn\"><a href=\"/\">Home</a></div>
               <div class=\"top-btn\"><a href=\"#top\">Top</a></div>
               <div class=\"footer-wrapper\">
               <div class=\"site-footer\">
                &copy xuchengpeng. <a href=\"/feed.xml\">RSS Feed</a>
               </div>
               </div>")))
