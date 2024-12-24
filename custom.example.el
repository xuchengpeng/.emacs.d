(setq user-full-name "Chuck"
      user-mail-address "330476629@qq.com")

(setq dotemacs-font (font-spec :family "BlexMono Nerd Font" :size 17)
      dotemacs-cn-font (font-spec :family "Microsoft Yahei"))

(setq dotemacs-package-archives 'custom)
(setq package-archives
      '(("gnu"   . "/path/to/elpa-mirror/gnu/")
        ("melpa" . "/path/to/elpa-mirror/melpa/")))

(setq dotemacs-tempel-path '("/path/to/dotfiles/tempel/templates"))

(setq dotemacs-org-blog-dir "/path/to/org-blog/"
      dotemacs-org-html-head "
<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"/apple-touch-icon.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"/favicon-32x32.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"/favicon-16x16.png\">
<link rel=\"manifest\" href=\"/site.webmanifest\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\">
<script src=\"/js/copycode.js\"></script>
<script src=\"https://cdn.jsdelivr.net/npm/jquery@3.7.1/dist/jquery.min.js\"></script>"
      dotemacs-org-html-preamble-format
      '(("en" "
<div class=\"header-wrapper\">
  <div class=\"site-header\">
  <a class=\"site-title\" href=\"/\">Chuck</a>
  <div class=\"site-nav\">
    <a class=\"nav-link\" href=\"/posts/\">Posts</a>
    <a class=\"nav-link\" href=\"/search/\">Search</a>
    <a class=\"nav-link\" href=\"/about/\">About</a>
  </div>
  </div>
</div>"))
      dotemacs-org-html-postamble-format
      '(("en" "
<div class=\"nav-btn\"><a href=\"/\">Home</a></div>
  <div class=\"top-btn\"><a href=\"#top\">Top</a></div>
  <div class=\"footer-wrapper\">
  <div class=\"site-footer\">&copy xuchengpeng. <a href=\"/feed.xml\">RSS Feed</a></div>
</div>")))
