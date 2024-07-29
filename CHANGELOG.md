# Changelog

## [2.2.5](https://github.com/xuchengpeng/.emacs.d/compare/v2.2.4...v2.2.5) (2024-07-29)


### Bug Fixes

* **ci:** add emacs 29.4 to the CI matrix ([1589ef4](https://github.com/xuchengpeng/.emacs.d/commit/1589ef4681c33d5420b9c7e30b29a5877ced5bad))
* **dired:** remove dired-rsync ([96f8069](https://github.com/xuchengpeng/.emacs.d/commit/96f806907910eb54dd3091de0796d5fd3349cfb0))
* **embark:** bind set-mark-command with general map ([f5011ee](https://github.com/xuchengpeng/.emacs.d/commit/f5011eeda0bc0c612099f33940c797a17890197a))
* **encoding:** don't set selection coding system on windows-nt ([e108d46](https://github.com/xuchengpeng/.emacs.d/commit/e108d46592aa09ba67473df6e9b5ac44a8214fe3))
* **flymake:** put margin indicator on the right ([6ce86f7](https://github.com/xuchengpeng/.emacs.d/commit/6ce86f78f99775c1b5f0ec0e654168504c0cd759))
* **git:** ignore elpa ([d731166](https://github.com/xuchengpeng/.emacs.d/commit/d731166e35438ec383783ebb0f46a64a33d84805))
* **ui:** enable tab-bar ([9c34dcf](https://github.com/xuchengpeng/.emacs.d/commit/9c34dcf268fde87d7ead24e470180b07cf679036))

## [2.2.4](https://github.com/xuchengpeng/.emacs.d/compare/v2.2.3...v2.2.4) (2024-05-25)


### Bug Fixes

* **ci:** remove deprecated release please action ([4940a1d](https://github.com/xuchengpeng/.emacs.d/commit/4940a1d652ddadbdb6c472e5561060f93a920188))
* **keybinds:** kill current buffer properly ([80710af](https://github.com/xuchengpeng/.emacs.d/commit/80710af6f06b5ad2d0daea4d3f02f12db6171ac4))
* **lua:** remove lua-ts-mode configuration ([6a0ac80](https://github.com/xuchengpeng/.emacs.d/commit/6a0ac80ce75052acf1dbfbd3b478e8fbd23b390b))
* **modeline:** fix right align size ([d1f93c9](https://github.com/xuchengpeng/.emacs.d/commit/d1f93c9fcea1e896addbf2a307f5413a1c2b0435))
* **symbol-overlay:** add keybinds in mode map ([850ef28](https://github.com/xuchengpeng/.emacs.d/commit/850ef28aeb4ac421354c48fd947c05aa13ed780d))
* **use-package:** ensure packages which need to be installed ([f2e424d](https://github.com/xuchengpeng/.emacs.d/commit/f2e424dc9673594f66b947d199604d5b3b0b6de8))

## [2.2.3](https://github.com/xuchengpeng/.emacs.d/compare/v2.2.2...v2.2.3) (2024-04-30)


### Bug Fixes

* **auto-save:** remove useless settings ([50ca4e3](https://github.com/xuchengpeng/.emacs.d/commit/50ca4e3cd7aa48c78c44312231d71e2ae89f044d))
* **ci:** no longer tag stable ([32ea583](https://github.com/xuchengpeng/.emacs.d/commit/32ea583a7d161032ffa6c04e90f85c205f22ab61))
* **completion:** disable Ispell completion function ([696ae08](https://github.com/xuchengpeng/.emacs.d/commit/696ae08f8195ea22cff473f23ddd52e11c4a6a85))
* **custom:** don't print message while loading ([4a02603](https://github.com/xuchengpeng/.emacs.d/commit/4a026031162979e30f86a36029c28d9cd9d099e0))
* **dabbrev:** ignore tags-table-mode ([a05b9e0](https://github.com/xuchengpeng/.emacs.d/commit/a05b9e00cd1422d87cfb2f5a33fa14cece5cb17c))
* **dashboard:** grep with consult-ripgrep ([65f3fa1](https://github.com/xuchengpeng/.emacs.d/commit/65f3fa1cb5dab519c7c00ffc6d587b59d9af4783))
* **eglot:** hover with eldoc-box ([c392112](https://github.com/xuchengpeng/.emacs.d/commit/c3921121af229fe8a46bc678738ab690c2de9950))
* **eglot:** remove obsolete variable ([ff102b6](https://github.com/xuchengpeng/.emacs.d/commit/ff102b603506f693a1e9b47da6af955914b6495d))
* **embark:** binding embark commands ([57af01f](https://github.com/xuchengpeng/.emacs.d/commit/57af01facde02bebab1ece2572cbda6e4e8344da))
* **eshell:** enable visual-line-mode ([b7b1f17](https://github.com/xuchengpeng/.emacs.d/commit/b7b1f170fc8ccef76949b3fcad1bedefb13bd781))
* **indent:** C/C++ mode indent with 4 ([63c7f7e](https://github.com/xuchengpeng/.emacs.d/commit/63c7f7e7c4254372fd39b7732ea46c10dd290751))
* **lang:** formatting with reformatter ([02dcd92](https://github.com/xuchengpeng/.emacs.d/commit/02dcd920a511bca030a1081fb2a3a1021d337b64))
* **lua:** define lua-format without treesit support checking ([6d718c0](https://github.com/xuchengpeng/.emacs.d/commit/6d718c0f2800deccb13b64c3818a9eb117177e88))
* **lua:** enable lus-ts-mode if available ([74bfd27](https://github.com/xuchengpeng/.emacs.d/commit/74bfd27b434d4a97e822f28de80822af5575fa12))
* **lua:** format buffer with stylua ([4f503a4](https://github.com/xuchengpeng/.emacs.d/commit/4f503a424c034c810c11d348dce36e18099605df))
* **markdown:** formatting with prettier ([d9cf134](https://github.com/xuchengpeng/.emacs.d/commit/d9cf134f06766aa2eadcb25ab8e112ee6fc81694))
* **markdown:** remove dotemacs-markdown-toc ([a2a12bd](https://github.com/xuchengpeng/.emacs.d/commit/a2a12bd868d2cb44d41461c5bbec38fd23a2c5fa))
* **modeline:** calculate string in pixel size ([5a2b2ad](https://github.com/xuchengpeng/.emacs.d/commit/5a2b2ad491e954f898bcbe2e5079cb903b035c57))
* **modeline:** change face of matches ([f81f01b](https://github.com/xuchengpeng/.emacs.d/commit/f81f01bc2042410dc6869edafcad05b763ecae37))
* **modeline:** change help-echo of flymake ([e45dd33](https://github.com/xuchengpeng/.emacs.d/commit/e45dd33d31c191d44569f901b149b0a8b9f7bed5))
* **modeline:** minor refactoring ([9985823](https://github.com/xuchengpeng/.emacs.d/commit/998582315a69567922bf477786426ff42fde9f9c))
* **modeline:** remove misc info ([b060c9a](https://github.com/xuchengpeng/.emacs.d/commit/b060c9a2d5bdd4e68e735cb3dfde62c7f5b4206f))
* **modeline:** remove position mouse map ([31b18c7](https://github.com/xuchengpeng/.emacs.d/commit/31b18c78aaa781475b313b591aedafadcc1214f2))
* **modeline:** show matches at end of left segments ([94834b0](https://github.com/xuchengpeng/.emacs.d/commit/94834b0f187c2915e10a12a2fe3b0639f8bd5e35))
* **modeline:** show matches with panel face ([7a89643](https://github.com/xuchengpeng/.emacs.d/commit/7a8964332e9be7f495f3f3ca35402f0c16628cf7))
* **modeline:** shrink relative path if too long ([ed37d28](https://github.com/xuchengpeng/.emacs.d/commit/ed37d28694c7621547414fed4e1ec567886eeb4a))
* **modeline:** simply text-scale ([8e0b831](https://github.com/xuchengpeng/.emacs.d/commit/8e0b831af007b1decfa09786b1870db885445cc5))
* **python:** format buffer with black ([96d4655](https://github.com/xuchengpeng/.emacs.d/commit/96d4655382e67efe33415ea683e7f1ef14ae3907))
* **reformatter:** add '-' argument ([c81c0f9](https://github.com/xuchengpeng/.emacs.d/commit/c81c0f95b6ec35cd0af2c47c21722ab45a17fd82))
* **saveplace:** custom save-place-file ealier ([2996f3e](https://github.com/xuchengpeng/.emacs.d/commit/2996f3eaf6893188e9452dff1920a6315dc1bd1c))
* **shfmt:** format with indent offset ([4e4338f](https://github.com/xuchengpeng/.emacs.d/commit/4e4338fa8c09a96bfaca887a776afecff7c38536))
* **symbol-overlay:** remove all with M-I ([c400b3c](https://github.com/xuchengpeng/.emacs.d/commit/c400b3c520c9dcd1a8873aa3eebfc177ea307e59))
* **ui:** remove tokyonight-themes ([13e7320](https://github.com/xuchengpeng/.emacs.d/commit/13e73209c617f9843adba742d99d8eed00a17650))
* **ui:** show buffer file name in frame title ([1677c78](https://github.com/xuchengpeng/.emacs.d/commit/1677c78346415335e2471dbad77e0704e8657cdc))
* **utils:** remove IS-XXX ([567b012](https://github.com/xuchengpeng/.emacs.d/commit/567b012742a4030935a961d0f6a548e35cace7a8))
* **vertico:** use default settings ([39aad84](https://github.com/xuchengpeng/.emacs.d/commit/39aad842c956057e5747eaa1fed7257514405244))
* **yaml:** formatting with prettier ([e3adfa1](https://github.com/xuchengpeng/.emacs.d/commit/e3adfa11ddc864c7b5eb68b9022e32897db863ed))

## [2.2.2](https://github.com/xuchengpeng/.emacs.d/compare/v2.2.1...v2.2.2) (2024-03-30)


### Bug Fixes

* **ci:** add emacs 29.3 to the CI matrix ([289c380](https://github.com/xuchengpeng/.emacs.d/commit/289c380a972a2eb00c2f0b53fcaf01a80de6dfdc))
* **consult:** better configuration ([ac34b87](https://github.com/xuchengpeng/.emacs.d/commit/ac34b873128b5698a849a2e40c28a2c168bbfb2c))
* **consult:** search symbol at point by default ([76eeea3](https://github.com/xuchengpeng/.emacs.d/commit/76eeea3a2d57cae5abf81aadcd8676cf62db9a29))
* **eglot:** remove consult-eglot ([fff1527](https://github.com/xuchengpeng/.emacs.d/commit/fff1527da7e6c5aa68d3fea6d0b9faa0d5976888))
* **embark:** load embark-consult ([eb184a2](https://github.com/xuchengpeng/.emacs.d/commit/eb184a2e284078a7e72ef4ce31133301549b0bdb))
* **gitignore:** ignore data files ([0a0ca15](https://github.com/xuchengpeng/.emacs.d/commit/0a0ca1514354673d3b27d4883d7aa81cb8f7a3b7))
* **keybinds:** bind grep commands ([e14ee6c](https://github.com/xuchengpeng/.emacs.d/commit/e14ee6c8f87566a44dc5ad12d39d8dc5266dd30f))
* **keybinds:** open diagnostic with consult-flymake ([7972954](https://github.com/xuchengpeng/.emacs.d/commit/797295436058bd17c7b491700f5908d2dbf6be25))
* **keybinds:** simply descriptions ([16443b6](https://github.com/xuchengpeng/.emacs.d/commit/16443b6319601b42fa4eb701e99bfd843ad32392))
* **modeline:** faces inherit from dotemacs-modeline ([c4ee8a7](https://github.com/xuchengpeng/.emacs.d/commit/c4ee8a737d110bb31dfb10eef949408810c252b8))
* **modeline:** simple buffer info ([8fcdc2c](https://github.com/xuchengpeng/.emacs.d/commit/8fcdc2c76ada608d40262c9ea78da272b79752b3))
* **shell:** use vterm with non-windows systems ([94d9592](https://github.com/xuchengpeng/.emacs.d/commit/94d9592450a386fb8172793fdf2ffdeff7371242))
* **tokyonight-themes:** proper which-key group description and key face ([67b8819](https://github.com/xuchengpeng/.emacs.d/commit/67b8819ccdfdc7189791367657d26b59c782945b))
* **treesit:** check if treesit available ([659b729](https://github.com/xuchengpeng/.emacs.d/commit/659b7293d7b3bd4ed503c7c2788ea317ff519e4c))
* **ui:** don't ring the bell ([84f68fe](https://github.com/xuchengpeng/.emacs.d/commit/84f68feac39af17be138797a356fc1f9099260a5))
* **vterm:** customize modeline style ([34001d8](https://github.com/xuchengpeng/.emacs.d/commit/34001d893f96e8fa2e97ce849496eceacd096def))
* **vterm:** show default directory in modeline ([c139c44](https://github.com/xuchengpeng/.emacs.d/commit/c139c44b99ea2d70ed3da33e903fb675f442f416))

## [2.2.1](https://github.com/xuchengpeng/.emacs.d/compare/v2.2.0...v2.2.1) (2024-02-29)


### Bug Fixes

* **custom:** custom.el example ([70407ff](https://github.com/xuchengpeng/.emacs.d/commit/70407ff361d6da1b37e5be2041dedec5f39fe9fb))
* **custom:** update example for ox-publish ([98d8f1f](https://github.com/xuchengpeng/.emacs.d/commit/98d8f1f56d20f2010db45b7f349e6cbeb383e307))
* **custom:** update example for ox-publish ([03f6079](https://github.com/xuchengpeng/.emacs.d/commit/03f60791639f50bafdfcc2ddbe7ed24c000ac652))
* **keybinds:** add LSP keymaps ([08394df](https://github.com/xuchengpeng/.emacs.d/commit/08394dff64c2bcc9acc8d9004b16ec28b0540903))
* **modeline:** add workspace-name via tab-bar ([132160c](https://github.com/xuchengpeng/.emacs.d/commit/132160ca43f33209bce28737b3286ad374fdf934))
* **modeline:** display flymake correctly ([0445710](https://github.com/xuchengpeng/.emacs.d/commit/0445710b3d297eac70cabe0e403b1f9bceebb5b5))
* **modeline:** refactoring ([c2643f2](https://github.com/xuchengpeng/.emacs.d/commit/c2643f26267b6cfa13b93e5fbcf161b1fe7e4d33))
* **modeline:** shorter matches info ([e512f35](https://github.com/xuchengpeng/.emacs.d/commit/e512f359e7f671241bc84c0b7d1a4772d660f2af))
* **multisession:** store files in cache directory ([3593a26](https://github.com/xuchengpeng/.emacs.d/commit/3593a2652c96bca849344a2048fc8bccc0993fde))
* **org:** add some languages for babel ([9c6d1a0](https://github.com/xuchengpeng/.emacs.d/commit/9c6d1a0e12c0693cb9fb46ff9e383cc11488d00b))
* **org:** publish ico files ([4462c6b](https://github.com/xuchengpeng/.emacs.d/commit/4462c6b260c479abd1d4cf8a091f06c72fcb5498))
* **ox-publish:** better indention ([6536d4c](https://github.com/xuchengpeng/.emacs.d/commit/6536d4c56ad26edad58c9edac86e57386e08f64a))
* **symbol-overlay:** inhibit symbol-overlay-map ([68273fb](https://github.com/xuchengpeng/.emacs.d/commit/68273fbb833b242ec8c1f47bfd53f31a39f48d55))
* **tokyonight-themes:** add support for 'completions-highlight' face ([1052ac7](https://github.com/xuchengpeng/.emacs.d/commit/1052ac785f57e9fc9b830912cfa7696f232acce8))
* **tokyonight-themes:** add which-key support ([5356fd4](https://github.com/xuchengpeng/.emacs.d/commit/5356fd46dba909086ac0dc9bf4c85a5c8f4cd655))
* **transient:** store files in cache directory ([15c6c02](https://github.com/xuchengpeng/.emacs.d/commit/15c6c02abb6d9ffb764d79b50e51731e12ffd4db))
* **treesit:** remove useless function ([332067f](https://github.com/xuchengpeng/.emacs.d/commit/332067f35f06ba4e31eb2aee3b1a9cced71a665f))
* **ui:** adjust background transparency ([5394f3f](https://github.com/xuchengpeng/.emacs.d/commit/5394f3f66866fa9974cee4f6a3efdac0f6817c92))
* **which-key:** customize display style ([5b2cc37](https://github.com/xuchengpeng/.emacs.d/commit/5b2cc3732a530b717dd6f2f8d175872adb6c65e5))
* **windmove:** drop windmove default keybindings ([ba15fc4](https://github.com/xuchengpeng/.emacs.d/commit/ba15fc467e0a2db8b71fb8828924e290768cd4ac))
* **workspaces:** add menu item name ([b33a6b8](https://github.com/xuchengpeng/.emacs.d/commit/b33a6b8aa6194101bea9442a9f70a014900970d8))
* **workspaces:** manage workspaces via tab-bar-mode ([7efcec9](https://github.com/xuchengpeng/.emacs.d/commit/7efcec9e5927039c86ad25e87a2dfd6ed494b54c))

## [2.2.0](https://github.com/xuchengpeng/.emacs.d/compare/v2.1.4...v2.2.0) (2024-01-31)


### Features

* **.emacs.d:** refactoring ([3aa54bc](https://github.com/xuchengpeng/.emacs.d/commit/3aa54bcd21b6063a126972f0686af83152f27b6a))


### Bug Fixes

* **.emacs.d:** minor refactoring ([c388949](https://github.com/xuchengpeng/.emacs.d/commit/c388949bf582e4a76515859f28664622cc5b7c59))
* **avy:** add keybinds for avy commands ([3a86ad5](https://github.com/xuchengpeng/.emacs.d/commit/3a86ad5b13d16aea2c6698a2c5d09e3921fb490a))
* **benchmark:** require on debug mode ([afda2ff](https://github.com/xuchengpeng/.emacs.d/commit/afda2ffee3688697d98624784cce45224dc9c3fc))
* **cape:** update the candidates for eglot ([4f3ea88](https://github.com/xuchengpeng/.emacs.d/commit/4f3ea883e1b32ef1b3b4cda0cf0e46328c3737d2))
* **ci:** add emacs 29.2 to the CI matrix ([0401a31](https://github.com/xuchengpeng/.emacs.d/commit/0401a31a580d815ed7f938a703f553a521bbfa27))
* **consult:** prefer bultin consult-fd ([77ba08e](https://github.com/xuchengpeng/.emacs.d/commit/77ba08ea2839a301c01d8bc0c5d19d9afd96a9ab))
* **core:** add dotemacs-delete-carrage-returns function ([5f27c73](https://github.com/xuchengpeng/.emacs.d/commit/5f27c733d9aecab667ad90ffb3f2b66482a5f835))
* **core:** improve performance ([e555c99](https://github.com/xuchengpeng/.emacs.d/commit/e555c99274f31016496405c63d41f00667b21d69))
* **core:** load windmove default keybindings after init ([6d94a45](https://github.com/xuchengpeng/.emacs.d/commit/6d94a456fcd08daa3c4e5ea624180ad95510484c))
* **core:** proper encoding ([d90669a](https://github.com/xuchengpeng/.emacs.d/commit/d90669a40fec4446cdf997fec00b4b090ce881f3))
* **core:** remove warnings ([c8f74b8](https://github.com/xuchengpeng/.emacs.d/commit/c8f74b8e29c057df062a40fd412a92f55c9f0024))
* **corfu:** customize delay options to default value ([379e682](https://github.com/xuchengpeng/.emacs.d/commit/379e68259ed9197de80c1569677d5fbc6f761689))
* **dabbrev:** use dabbrev with corfu ([17e8f1e](https://github.com/xuchengpeng/.emacs.d/commit/17e8f1e1208472c4c48fcd7b085b5e0bc048243f))
* **edit:** enable useful modes after init ([461ca71](https://github.com/xuchengpeng/.emacs.d/commit/461ca7117c4475886a5c0cd13d54bea3434658da))
* **eglot:** don't enable with lisp-data-mode ([10e369a](https://github.com/xuchengpeng/.emacs.d/commit/10e369a58a023da08dd311de2af7374420784338))
* **eglot:** remap xref-find-apropos with consult-eglot-symbols ([c8cda39](https://github.com/xuchengpeng/.emacs.d/commit/c8cda394e2099e47a3c8bf8bb64a3da7bd00b139))
* **emacs:** increase gc-cons-threshold ([ca8408e](https://github.com/xuchengpeng/.emacs.d/commit/ca8408ee5a82a3a5372b68e3d51ceb0fa086ff32))
* **expreg:** replace expand-region ([e358b9c](https://github.com/xuchengpeng/.emacs.d/commit/e358b9c9193ce18ea53d19b8ba4bfc565c2acbd3))
* **flyspell:** enable if found aspell ([1601c2a](https://github.com/xuchengpeng/.emacs.d/commit/1601c2ada82298f2baa8afd8421e1d7ed95fb683))
* **flyspell:** manually enabled ([b2af073](https://github.com/xuchengpeng/.emacs.d/commit/b2af073ef46918753b51f911b9017d48b8aadb91))
* **hl-line:** disable in shell&dashboard ([de35f60](https://github.com/xuchengpeng/.emacs.d/commit/de35f60f3ff9316f03b979a5f182352a7b3bcb32))
* **keybinds:** bind consult-flymake with '&lt;leader&gt; o f' ([91cdc6f](https://github.com/xuchengpeng/.emacs.d/commit/91cdc6fcb3f7f0ff2a9a0204e752d59c438cf2d0))
* **keybinds:** new keybinding functions with emacs-29 ([0e158db](https://github.com/xuchengpeng/.emacs.d/commit/0e158dbc1aa9bc94fbc27c6c58dd4baadac41ee5))
* **license:** update year of license ([f11a1ed](https://github.com/xuchengpeng/.emacs.d/commit/f11a1ed91930b50d10d37894dd69368c4ea17068))
* **markdown:** preview with pandoc ([0882c18](https://github.com/xuchengpeng/.emacs.d/commit/0882c18a5de71aff5f04bbaed8e69f179e5e2b1e))
* **markdown:** remove markdown-toc package ([6bf5002](https://github.com/xuchengpeng/.emacs.d/commit/6bf5002d7af98241d7b9ef8b8a63f101a3e7d175))
* **modeline:** add shell style ([4dc5fe7](https://github.com/xuchengpeng/.emacs.d/commit/4dc5fe70297b768a48701738a3d6df5c5ec90aab))
* **modeline:** fix nil value ([fda5646](https://github.com/xuchengpeng/.emacs.d/commit/fda56469bc92884a16b99de747d1ed451fc1f5df))
* **modeline:** remove warnings ([84cb379](https://github.com/xuchengpeng/.emacs.d/commit/84cb3791b3b2b62a9c79fba189b2deb4b81cdd2d))
* **modules:** refactoring ([c92d9f8](https://github.com/xuchengpeng/.emacs.d/commit/c92d9f8d73c40a589e0add8716a50922fc0c7a83))
* **org:** add capture template for journal ([394597e](https://github.com/xuchengpeng/.emacs.d/commit/394597e214d584969f591a985d60d090552d3ee3))
* **org:** add keybinds ([8d89bef](https://github.com/xuchengpeng/.emacs.d/commit/8d89bef7ac8d59cc8177b2b83d68cd482e85b62d))
* **org:** disable electric-indent for org files ([73ce6f3](https://github.com/xuchengpeng/.emacs.d/commit/73ce6f37b729fa15d9272689d05fb89d814e6729))
* **org:** generate new post in current month directory ([c27bc20](https://github.com/xuchengpeng/.emacs.d/commit/c27bc2063b53f1bbaccace1f6724b12435272608))
* **ox-publish:** add site directory configuration ([fdb8687](https://github.com/xuchengpeng/.emacs.d/commit/fdb8687942d1299245496825730b0c7aa4da3c66))
* **packages:** refactoring ([7067635](https://github.com/xuchengpeng/.emacs.d/commit/70676358dd531a635bb808d12d1ab9c03f2d463b))
* **packages:** refactoring ([693356b](https://github.com/xuchengpeng/.emacs.d/commit/693356b88afd31696d23e84340ec61ddb1528d48))
* **perf:** save startup time ([a26e2ef](https://github.com/xuchengpeng/.emacs.d/commit/a26e2efed0cda20832b19b6349bfa2fda31dfbc5))
* **project:** put project-list-file in cache directory ([c67efaf](https://github.com/xuchengpeng/.emacs.d/commit/c67efaff85a9f199796f12d01319c9cfed2696ef))
* **readme:** add badge for emacs ([f05cadf](https://github.com/xuchengpeng/.emacs.d/commit/f05cadff36c75441d2adb65385d2db730673732f))
* **savehist:** set history-length with 1000 ([d94564a](https://github.com/xuchengpeng/.emacs.d/commit/d94564a6450807876acf965b1ae6480f900eb731))
* **text-mode:** enable visual-line-mode ([08660c5](https://github.com/xuchengpeng/.emacs.d/commit/08660c579dca34b97c9f772db0a61e21190363ba))
* **themes:** disable enabled themes before load new theme ([46f0580](https://github.com/xuchengpeng/.emacs.d/commit/46f0580f12ab06d654c3e87dddfa3ba24214f573))
* **themes:** prefer modus-themes ([bebcde4](https://github.com/xuchengpeng/.emacs.d/commit/bebcde4475ba9c6aac6c0f19096d9acd0287b13a))
* **tokyonight-themes:** add ace-window support ([d3b4f51](https://github.com/xuchengpeng/.emacs.d/commit/d3b4f51b244909bfa3d7bc2b1d885866be34afd8))
* **tokyonight-themes:** add ansi-color faces ([a498e3a](https://github.com/xuchengpeng/.emacs.d/commit/a498e3ae5ba891e22ca04004b73d81b028e71dfe))
* **tokyonight-themes:** add avy support ([e7d9c37](https://github.com/xuchengpeng/.emacs.d/commit/e7d9c377eabdb8d1f234e3b2633104bf2a14cac6))
* **tokyonight-themes:** add basic faces ([d21adbb](https://github.com/xuchengpeng/.emacs.d/commit/d21adbb281e7243c59248b9049adc37741046af6))
* **tokyonight-themes:** add bookmark faces ([43d9313](https://github.com/xuchengpeng/.emacs.d/commit/43d9313cc56a6f00a850b568a91c4dbd96b01e44))
* **tokyonight-themes:** add builtin faces ([3a97e68](https://github.com/xuchengpeng/.emacs.d/commit/3a97e68f7a372b138af7f7a677c4fafe746f42cf))
* **tokyonight-themes:** add calendar and diary faces ([b2f8aee](https://github.com/xuchengpeng/.emacs.d/commit/b2f8aeee54de9396788557934403b2856823747c))
* **tokyonight-themes:** add custom faces ([6c100c6](https://github.com/xuchengpeng/.emacs.d/commit/6c100c68c8724ad7932ad3c0e4a457b71334a7c3))
* **tokyonight-themes:** add diff-mode faces ([3d8ae36](https://github.com/xuchengpeng/.emacs.d/commit/3d8ae36f378d025bcf71e6ae88ad36febe70e1df))
* **tokyonight-themes:** add dired faces ([aae321d](https://github.com/xuchengpeng/.emacs.d/commit/aae321d6936e1dc56c9eb115de89d46422fc6a49))
* **tokyonight-themes:** add flymake support ([102e87d](https://github.com/xuchengpeng/.emacs.d/commit/102e87d27404826216753b75c446661ce10bb90a))
* **tokyonight-themes:** add hl-todo faces ([a78da72](https://github.com/xuchengpeng/.emacs.d/commit/a78da727a895be8aaf3ac46d4dfd1f47db50c051))
* **tokyonight-themes:** add icomplete faces ([ce35c5f](https://github.com/xuchengpeng/.emacs.d/commit/ce35c5f5a5034faf4c199db5456930565ceb02c3))
* **tokyonight-themes:** add ido faces ([79f7cf1](https://github.com/xuchengpeng/.emacs.d/commit/79f7cf18cf8bc6cbe4f4e1a2efe2d6f84e279a4b))
* **tokyonight-themes:** add message faces ([1f115ee](https://github.com/xuchengpeng/.emacs.d/commit/1f115ee306861c284f1c0936ab6412219c7dc6c0))
* **tokyonight-themes:** add multiple-cursors support ([025bc73](https://github.com/xuchengpeng/.emacs.d/commit/025bc737438e2d25b4738182d0ad9770713d2b68))
* **tokyonight-themes:** add orderless faces ([56a80d9](https://github.com/xuchengpeng/.emacs.d/commit/56a80d9256a93d48402fd254d0c9763490fedee1))
* **tokyonight-themes:** add symbol-overlay support ([7b793c1](https://github.com/xuchengpeng/.emacs.d/commit/7b793c120308a2d18eb0f0eb55a9c1a2b3cdee86))
* **tokyonight-themes:** add tokyonight-themes-with-colors ([63591c1](https://github.com/xuchengpeng/.emacs.d/commit/63591c1071bba1348af69d71f4f8bfa07f1de3b6))
* **tokyonight-themes:** add vundo support ([266dab2](https://github.com/xuchengpeng/.emacs.d/commit/266dab20b969253b3f410ba949fe874bb54cb64a))
* **tokyonight-themes:** customize font-lock&compilation faces ([25aad92](https://github.com/xuchengpeng/.emacs.d/commit/25aad9261c372f2a301cbb05f15419abe2085928))
* **tokyonight-themes:** make cursor clearly with day style ([509340f](https://github.com/xuchengpeng/.emacs.d/commit/509340f40f588fb5774796c9940f50e05992312a))
* **tokyonight-themes:** proper isearch faces ([3727cd6](https://github.com/xuchengpeng/.emacs.d/commit/3727cd6d7165a6cc472737e3d55321bb407bcd56))
* **tokyonight-themes:** proper line-number ([24d6936](https://github.com/xuchengpeng/.emacs.d/commit/24d6936fde415f22b97aff970a062cd395544386))
* **tokyonight-themes:** proper mode-line faces ([c845d9e](https://github.com/xuchengpeng/.emacs.d/commit/c845d9e2cf1a5d066c09f1513d6d98e178dc9df7))
* **tokyonight-themes:** refactoring ([4a533ce](https://github.com/xuchengpeng/.emacs.d/commit/4a533cee638c253761e70a32b9ef5dbdd835cfe8))
* **tokyonight-themes:** use right holiday face ([c081f30](https://github.com/xuchengpeng/.emacs.d/commit/c081f30729e72f4171fa728a1d1251703633eba4))
* **treesit:** add native treesit support ([1a6e37c](https://github.com/xuchengpeng/.emacs.d/commit/1a6e37c3c47119a51010392e7055c778135d57ec))
* **treesit:** void-function treesit-ready-p ([c0211b2](https://github.com/xuchengpeng/.emacs.d/commit/c0211b2aee6a463940dce599196ec67fab9cac4a))
* **ui:** blink the mode-line ([f6811b3](https://github.com/xuchengpeng/.emacs.d/commit/f6811b3ecad764d8b61f45e2c6b4822ea24fd9ed))
* **ui:** enable pixel-scroll ([ce0998b](https://github.com/xuchengpeng/.emacs.d/commit/ce0998ba4c50e023c3fe8b821a8eee220ec8ad57))
* **ui:** set dotemacs-cn-font with han ([c673d32](https://github.com/xuchengpeng/.emacs.d/commit/c673d3211c07ab508f1e0857398a6e469c684413))
* **vertico:** convenient selection ([5a45940](https://github.com/xuchengpeng/.emacs.d/commit/5a45940cc4eca08f9946e76c21dbf65210c26770))
* **which-key:** enable after init ([d7acfd3](https://github.com/xuchengpeng/.emacs.d/commit/d7acfd3eddb8000ee924f66c2a190a00b59ac036))
* **whitespace:** delete trailing whitespace before save ([1da3214](https://github.com/xuchengpeng/.emacs.d/commit/1da32145eba8424c073929969673e348630758b2))
* **xref:** simply customization ([c8c5326](https://github.com/xuchengpeng/.emacs.d/commit/c8c5326f79e99b1dcad6517a04175304b7c81372))

## [2.1.4](https://github.com/xuchengpeng/.emacs.d/compare/v2.1.3...v2.1.4) (2023-12-28)


### Bug Fixes

* **org:** add post capture template ([cd4c9fb](https://github.com/xuchengpeng/.emacs.d/commit/cd4c9fb690d90150d7b46e4c9f0a643110720499))
* **ox-publish:** add custom variables ([7ddce14](https://github.com/xuchengpeng/.emacs.d/commit/7ddce149fdbd4b882332a1f9ee4e9da0795eeea1))
* **ox-publish:** customize html parameters default values ([4829df5](https://github.com/xuchengpeng/.emacs.d/commit/4829df5c22a52b04e867612ad40dffc45de494df))
* **ox-publish:** do not export timestamp ([44878cb](https://github.com/xuchengpeng/.emacs.d/commit/44878cb9114ace8b0184aea67162d0e06eb37d41))
* **ox-publish:** export with toc ([3293ce2](https://github.com/xuchengpeng/.emacs.d/commit/3293ce296b6d65a3940552fcee5376c6e66804a5))
* **ox-publish:** use backquote to eval site directory ([f6c3a2f](https://github.com/xuchengpeng/.emacs.d/commit/f6c3a2fd849878b08ab306639b02fb036881a9c9))
* **ox-publish:** use new HTML5 elements ([62e3b85](https://github.com/xuchengpeng/.emacs.d/commit/62e3b853a23bbbc4a4e92b1133c97c9b98de7f62))

## [2.1.3](https://github.com/xuchengpeng/.emacs.d/compare/v2.1.2...v2.1.3) (2023-12-26)


### Bug Fixes

* **corfu:** add tempel support ([c0934fc](https://github.com/xuchengpeng/.emacs.d/commit/c0934fcf9d496f02417a9ed38b3df737623ddff1))
* **modeline:** prefer ace-window insead of winum ([6147570](https://github.com/xuchengpeng/.emacs.d/commit/614757016bc8d9c304fe2ce4ab7dde12baa6e8a9))
* **modeline:** refactoring ([c3815a7](https://github.com/xuchengpeng/.emacs.d/commit/c3815a7d813da2a6575c99e5ec84359a6da6528c))
* **symbol-overlay:** add keybinds ([c3171fc](https://github.com/xuchengpeng/.emacs.d/commit/c3171fca95704ec664eedd956b7b11d6833b3f86))
* **use-package:** avoid using bind ([71fa61d](https://github.com/xuchengpeng/.emacs.d/commit/71fa61da8f8b1af39f637a57d85d49dbfeed0e25))

## [2.1.2](https://github.com/xuchengpeng/.emacs.d/compare/v2.1.1...v2.1.2) (2023-12-25)


### Bug Fixes

* **package:** refactoring ([6007d61](https://github.com/xuchengpeng/.emacs.d/commit/6007d6143ed598fc02021138a22566728f6054f2))
* **package:** remove straight.el ([759bbd5](https://github.com/xuchengpeng/.emacs.d/commit/759bbd5c99bca21cc47d3689f2abb7d9a2c11817))
* **tabs:** remove tabs ([58ec4ad](https://github.com/xuchengpeng/.emacs.d/commit/58ec4adcfbca49b93a10444b0ebc912b6b79ea23))

## [2.1.1](https://github.com/xuchengpeng/.emacs.d/compare/v2.1.0...v2.1.1) (2023-12-22)


### Bug Fixes

* **corfu:** customize popupinfo delay ([03ed2e3](https://github.com/xuchengpeng/.emacs.d/commit/03ed2e3e76ad00eee4f57df08db96ccb6fcf9656))
* **corfu:** replace company ([540fe4c](https://github.com/xuchengpeng/.emacs.d/commit/540fe4c3751668938e430bbac6799f7976ea9108))
* **dired:** add rsync support ([0210d82](https://github.com/xuchengpeng/.emacs.d/commit/0210d8261bfdb02e6768f6437569cde139a89f7f))
* **dired:** reuse current buffer by pressing 'a' ([6926f74](https://github.com/xuchengpeng/.emacs.d/commit/6926f741a72cc9d15fd83ea92a611bc074132436))
* **eglot:** customize code keymaps ([bdedbae](https://github.com/xuchengpeng/.emacs.d/commit/bdedbae7ad57873f79c8b2003e774b91a7f7bb65))
* **eldoc-box:** add eldoc-box with eglot ([8bcd7ad](https://github.com/xuchengpeng/.emacs.d/commit/8bcd7ad13a59da6886ab9e70e78543ab61a7550f))
* **electric-pair:** replace smartparens with electric-pair ([fc1fa3b](https://github.com/xuchengpeng/.emacs.d/commit/fc1fa3b5effc634b8233854867d91e72b772f0b5))
* **flycheck:** use right fringe ([21c2d21](https://github.com/xuchengpeng/.emacs.d/commit/21c2d21db4e62437b5c673c8b0cbcb064b19b360))
* **flymake:** replace flycheck ([c621c61](https://github.com/xuchengpeng/.emacs.d/commit/c621c61d9d08a1f4d560bfb4846cad39ba8feb8b))
* **lsp-mode:** refactoring ([5a55e8a](https://github.com/xuchengpeng/.emacs.d/commit/5a55e8acd1640e00770b474085ef5bfcd6944530))
* **lsp:** disable some features ([771694d](https://github.com/xuchengpeng/.emacs.d/commit/771694dbd353280e548c5408d2b0293752b7a15c))
* **lsp:** prefer eglot as default lsp server ([1952ac2](https://github.com/xuchengpeng/.emacs.d/commit/1952ac2b942737e05ac6083ca0f099219ecccab2))
* **projectile:** prefer project instead of projectile ([3116ff1](https://github.com/xuchengpeng/.emacs.d/commit/3116ff18cde0ead4ffab01c3e3aa443656573fb7))
* **project:** project-prefix-map bind with (C-x p) by default ([7d0de63](https://github.com/xuchengpeng/.emacs.d/commit/7d0de63de5f605532f12534fdec16350d14c94db))
* **themes:** set tokyonight-moon as default style ([d74e86b](https://github.com/xuchengpeng/.emacs.d/commit/d74e86ba9944e61234d7910f2fc4239c728ec563))
* **tokyonight:** add corfu support ([b6e0970](https://github.com/xuchengpeng/.emacs.d/commit/b6e0970ace9e2c3e53717503784c040decb5b88f))
* **undo:** replace undo-tree with vundo ([988fe14](https://github.com/xuchengpeng/.emacs.d/commit/988fe1419dd592134ba2ecdb524dd81e05ab351a))
* **use-package:** install from mirror ([5f83d1f](https://github.com/xuchengpeng/.emacs.d/commit/5f83d1fb6be3b33a4713bd8837dc72a3520180ad))
* **vertico:** simply installation ([76c1ea8](https://github.com/xuchengpeng/.emacs.d/commit/76c1ea8b492e5efaad060a6aeb7872a8a151464a))
* **xref:** improvement ([f024d32](https://github.com/xuchengpeng/.emacs.d/commit/f024d322820e6ddaa78d0d646bcbbbe616349bf5))

## [2.1.0](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.12...v2.1.0) (2023-12-19)


### Features

* **lsp:** add lsp-mode support ([2df6ff8](https://github.com/xuchengpeng/.emacs.d/commit/2df6ff8131585ce314b290dcc5136031f671213d))


### Bug Fixes

* **autorevert:** enable after init ([6fb16bd](https://github.com/xuchengpeng/.emacs.d/commit/6fb16bd68bcd6ed029980fbde5047066e3ede20c))
* **core:** remove unused function ([46b1384](https://github.com/xuchengpeng/.emacs.d/commit/46b13848cbc20d63e4f7c13109c90eb885925f16))
* **dired:** makeing dired pretty ([8b49ba9](https://github.com/xuchengpeng/.emacs.d/commit/8b49ba981e812975fe5f7f03f47c21a13b1779a7))
* **git:** remove magit ([3ad7419](https://github.com/xuchengpeng/.emacs.d/commit/3ad7419e49276b270bc65390e306463740ec17fa))
* **keybinds:** add lsp keymap ([07a37d8](https://github.com/xuchengpeng/.emacs.d/commit/07a37d842ec177ef96c4d91dd3853c06923fc2fe))
* **keybinds:** bind major mode map to M-SPC m ([8235ce9](https://github.com/xuchengpeng/.emacs.d/commit/8235ce933f357cfad3aeef81ab4b40e80efbd7ee))
* **lsp:** disable features that have great potential to be slow ([24272fe](https://github.com/xuchengpeng/.emacs.d/commit/24272feaf6cc644ca94307c803799dcec19ee793))
* **lsp:** enable modeline ([6442de7](https://github.com/xuchengpeng/.emacs.d/commit/6442de747b968eb7eb27d8d14c1eb329563193b6))
* **lua:** enable lsp after lua-mode ([47af6d1](https://github.com/xuchengpeng/.emacs.d/commit/47af6d1ff2037a862135bbd4e844bace3a858571))
* **org:** remove org-roam ([2cddcac](https://github.com/xuchengpeng/.emacs.d/commit/2cddcace6226dea08b85b4c87122525832f61df5))
* **pyright:** customize python interpreter by youself ([a8ffaf6](https://github.com/xuchengpeng/.emacs.d/commit/a8ffaf6e3f9b9b1f5c641d240d17a3d09520e1e1))
* **pyright:** symbol's value is void while init ([480923f](https://github.com/xuchengpeng/.emacs.d/commit/480923f430b6ec7cb3c601c85ee30e3824c54178))
* **python:** prefer pyright as language server ([58540d7](https://github.com/xuchengpeng/.emacs.d/commit/58540d722d1d7ee1a3de5b7a0189431e46195aa8))
* **tabs:** disable tabs by default ([8eb4d47](https://github.com/xuchengpeng/.emacs.d/commit/8eb4d47789f743927ae7b3a3df84e0ea42aefc4b))
* **tree-sitter:** remove tree-sitter ([bf5eb24](https://github.com/xuchengpeng/.emacs.d/commit/bf5eb24a72565441b8d309e47b6564b27b369221))
* **treemacs:** remove treemacs ([8f83627](https://github.com/xuchengpeng/.emacs.d/commit/8f836277cf2ab95245e0d29777bbb8c659f8b098))
* **vertico:** proper consult-fd ([461faf2](https://github.com/xuchengpeng/.emacs.d/commit/461faf2f4528c29641bab55caebeb600460f7e84))

## [2.0.12](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.11...v2.0.12) (2023-12-18)


### Bug Fixes

* **ci:** add emacs 29.1 to the CI matrix ([721a584](https://github.com/xuchengpeng/.emacs.d/commit/721a58437d59101ec181acb37c5d1b9076115320))
* **org:** bump release 9.6.13 ([40242ae](https://github.com/xuchengpeng/.emacs.d/commit/40242aeb5faabf6c5197d8194fd5c6b46bf5d32a))

## [2.0.11](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.10...v2.0.11) (2023-06-27)


### Bug Fixes

* **org:** bump release 9.6.7 ([47632a1](https://github.com/xuchengpeng/.emacs.d/commit/47632a163813aac0148cf92abc334f8e64e0987f))

## [2.0.10](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.9...v2.0.10) (2023-06-06)


### Bug Fixes

* **tokyonight:** region background diff from highlight ([366ea8b](https://github.com/xuchengpeng/.emacs.d/commit/366ea8bbb896e8e84cf8c8f77d135123e58809a2))

## [2.0.9](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.8...v2.0.9) (2023-05-27)


### Bug Fixes

* **company:** disabled in shell ([95bd317](https://github.com/xuchengpeng/.emacs.d/commit/95bd31762416d7bef4863a6100b84808a6947ba9))
* **modeline:** add padding for process ([2fb3ead](https://github.com/xuchengpeng/.emacs.d/commit/2fb3ead67f170918d92065ca4503a3b3b7faa069))
* **org:** use org-roam to take notes ([6e59e9b](https://github.com/xuchengpeng/.emacs.d/commit/6e59e9b22a8b8560670cb0af5edc744d37f1c275))
* **themes:** load tokyonight styles with command ([a151faa](https://github.com/xuchengpeng/.emacs.d/commit/a151faae913f002f5b7428f6870031855b040732))
* **tokyonight:** add completions faces ([2bb355e](https://github.com/xuchengpeng/.emacs.d/commit/2bb355eda3bdba64a5bee65e5713586632cc9027))
* **tokyonight:** add hl-line face ([4345cf7](https://github.com/xuchengpeng/.emacs.d/commit/4345cf7b04af9e0bedcffc3f6bba1384a4e1cfaa))
* **tokyonight:** customize company scrollbar faces ([a7bff3f](https://github.com/xuchengpeng/.emacs.d/commit/a7bff3f36359ff239d6eb7e7f7944d6bb64b3cd1))
* **tokyonight:** set flycheck background unspecified ([82fe46d](https://github.com/xuchengpeng/.emacs.d/commit/82fe46dbcdff41e6c5735bef78fcb223a35f8398))
* **tokyonight:** set minibuffer-prompt background unspecified ([1f81dbe](https://github.com/xuchengpeng/.emacs.d/commit/1f81dbea86e10a3ce0731ffd786c086b960e5f14))
* **undo-tree:** do not auto save history ([315be25](https://github.com/xuchengpeng/.emacs.d/commit/315be254f7d1bdc160bc75fe63e5b0e89c4a0a2a))

## [2.0.8](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.7...v2.0.8) (2023-05-24)


### Bug Fixes

* add more search commands ([d92d751](https://github.com/xuchengpeng/.emacs.d/commit/d92d7514adc7606e647370a9b76ba52f0ad01d87))
* **dashboard:** dashboard like alpha-nvim ([cc7990f](https://github.com/xuchengpeng/.emacs.d/commit/cc7990fcd58a92bcb8d599dcd82b716dcecf8595))
* **flyspell:** use aspell as backend ([16658af](https://github.com/xuchengpeng/.emacs.d/commit/16658af669949f5833f2a1ef8babaaa4ad7e1a7a))
* improve performance ([cfe6b03](https://github.com/xuchengpeng/.emacs.d/commit/cfe6b0339b7fc63271c6d018780141b3f1c04115))
* **keybinds:** add mode toggle keybinds ([ee78e5d](https://github.com/xuchengpeng/.emacs.d/commit/ee78e5d48e05a3e8efdb62979decf745a56cbf7c))
* reasonable defaults ([0603f3a](https://github.com/xuchengpeng/.emacs.d/commit/0603f3a551025cbc5c99eec80348e133ad49ecd3))
* **recentf:** active after init ([6f7e3f2](https://github.com/xuchengpeng/.emacs.d/commit/6f7e3f297bfc2b3262c52e6fdc66ee56b9aae060))
* **tokyonight:** add eshell faces ([569a031](https://github.com/xuchengpeng/.emacs.d/commit/569a031a5653af45a9b39a1041c951d8bae74bfd))
* **tokyonight:** add theme styles ([2a6a51c](https://github.com/xuchengpeng/.emacs.d/commit/2a6a51c8a62527ffdebd4947cc3e9e9e7c5764be))
* **tokyonight:** do not highlight current line number ([f3cc909](https://github.com/xuchengpeng/.emacs.d/commit/f3cc909f983fa6a82c4c0860eaabe7292d73b6bf))
* **tokyonight:** fix centaur-tabs style ([3094ac2](https://github.com/xuchengpeng/.emacs.d/commit/3094ac249f46c88ecf75cf841ce32dc6b6e0736c))
* **vertico:** customize consult-fd with args ([bbf6b39](https://github.com/xuchengpeng/.emacs.d/commit/bbf6b392944fc6f69a284ff2bc172ea0d163ce26))

## [2.0.7](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.6...v2.0.7) (2023-05-22)


### Bug Fixes

* **auto-save:** disabled by default ([80ac548](https://github.com/xuchengpeng/.emacs.d/commit/80ac548dc8200350a07fd58e064b7b1d510c4bfa))
* **company:** set local company backends after change major mode ([0791240](https://github.com/xuchengpeng/.emacs.d/commit/07912402c476cea877251d13010c3ed8987395c4))
* **display-line-numbers:** enable on prog-mode,conf-mode ([c00a412](https://github.com/xuchengpeng/.emacs.d/commit/c00a4128320d5323d2983c813c5d0687d576b0d8))
* **editor:** use move-text to move current line or region up or down ([bc6a86c](https://github.com/xuchengpeng/.emacs.d/commit/bc6a86cb8f8e24a65c4cd6c05fd7efc88f365bc3))
* **eshell:** customize built-in eshell ([b5d24ea](https://github.com/xuchengpeng/.emacs.d/commit/b5d24ea265e8a1e333427842c8ca092599ac18d5))
* **keybinds:** add explorer&terminal keybinds ([2c8cc2d](https://github.com/xuchengpeng/.emacs.d/commit/2c8cc2dd8dd8adee5dbc6da3d85c0a5498553b29))
* **lang:** add lua-mode ([44c93ff](https://github.com/xuchengpeng/.emacs.d/commit/44c93ff592dc8572a21bca566fe15b8ba73db015))
* **markdown:** bind local leader with markdown-mode-style-map ([4ccf2ed](https://github.com/xuchengpeng/.emacs.d/commit/4ccf2ed6118409d85135fb20821ee3bc674891d3))
* **markdown:** customize markdown command ([db79388](https://github.com/xuchengpeng/.emacs.d/commit/db79388150fb3e36d6207ee9324e6cde02a7b266))
* **org:** customize agenda,capture,publish-project ([82ee7c6](https://github.com/xuchengpeng/.emacs.d/commit/82ee7c641e29addfffce7e41e83d0d3d8c1c4eb5))

## [2.0.6](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.5...v2.0.6) (2023-05-19)


### Bug Fixes

* add file grep keybinding ([f9ef401](https://github.com/xuchengpeng/.emacs.d/commit/f9ef40141e2ed13249afbc303bcd5f8a30a8a35f))
* **lang:** add markdown-mode,markdown-toc ([e63ca1f](https://github.com/xuchengpeng/.emacs.d/commit/e63ca1f161da977cd0c5757791008c7d136caceb))
* **modeline:** add compilation status ([6dac6ac](https://github.com/xuchengpeng/.emacs.d/commit/6dac6ac475da82a289238e524fde48d27f9fdc69))
* **modeline:** refactoring core library ([1ce141a](https://github.com/xuchengpeng/.emacs.d/commit/1ce141a2399dd4e374c72ea9f87327f549b3145e))
* **modeline:** show misc info ([89127fe](https://github.com/xuchengpeng/.emacs.d/commit/89127fe3713a0fbdd967d1c633f67a51c982dfdb))
* set lexical-binding ([c704ffe](https://github.com/xuchengpeng/.emacs.d/commit/c704ffe1d15fe72a86423c5841eeaa36f104389c))
* **tokyonight:** customize font-lock faces ([6d4fb48](https://github.com/xuchengpeng/.emacs.d/commit/6d4fb488d6021615228c21ecbfb8edfcfd49f257))

## [2.0.5](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.4...v2.0.5) (2023-05-18)


### Bug Fixes

* **consult:** find files using fd ([89d7531](https://github.com/xuchengpeng/.emacs.d/commit/89d75317ce4bdb7b5ba6ac2bb81b30809b08106e))
* **editor:** add multiple cursors ([0661aed](https://github.com/xuchengpeng/.emacs.d/commit/0661aedcbc2b92546bd873bd6e83e0e9575116a7))
* **expand-region:** bind with C-= ([6bdfd8c](https://github.com/xuchengpeng/.emacs.d/commit/6bdfd8cd32abbba431bdd65284110ed769ec94b5))
* **mark:** customize mark keybinds ([f511feb](https://github.com/xuchengpeng/.emacs.d/commit/f511feb2167148aff40c5cb9066f0234cb320153))
* **modeline:** add mouse face and map ([8ad848a](https://github.com/xuchengpeng/.emacs.d/commit/8ad848a28b6b2be778a807fffb2b2d6cea9a3996))
* **modeline:** set multiple-cursors matches face ([d88bbf9](https://github.com/xuchengpeng/.emacs.d/commit/d88bbf95205dd3ffe439bfc306317dcb886dd960))
* **themes:** use tokyonight as default ([9a59752](https://github.com/xuchengpeng/.emacs.d/commit/9a597522363a2486e8639b6a6c465b15bbb7d442))
* **tokyonight:** add centaur-tabs faces ([12bbabd](https://github.com/xuchengpeng/.emacs.d/commit/12bbabdbfc0c332983979635b6728fea00d3cbc4))
* **tokyonight:** add company faces ([d4a79c7](https://github.com/xuchengpeng/.emacs.d/commit/d4a79c772b64d991ab5e7692568f462acd67ac4a))
* **tokyonight:** add flycheck faces ([9544dcd](https://github.com/xuchengpeng/.emacs.d/commit/9544dcd1fff8e798e88cb430c943ecae4392b4f7))
* **tokyonight:** add search faces ([15763c1](https://github.com/xuchengpeng/.emacs.d/commit/15763c19094ac56005beba2597dcefb3e0c7ebcb))
* **tokyonight:** add vertico-current face ([a2fa0f8](https://github.com/xuchengpeng/.emacs.d/commit/a2fa0f8e1f14a5b5f5bcac49c9176d8653122afa))
* **tokyonight:** customize search foreground color ([a2a13d0](https://github.com/xuchengpeng/.emacs.d/commit/a2a13d03d9936296076042a862e038f5963e5fdb))
* **treemacs:** do not show line numbers ([2bd402b](https://github.com/xuchengpeng/.emacs.d/commit/2bd402b39e6d560188b7caa84693b7ae90adc7d8))
* **undo-tree:** do not show line numbers ([396b30a](https://github.com/xuchengpeng/.emacs.d/commit/396b30a54b6f9644365552611d769c0a2f3544a0))
* **vertico:** add marginalia ([a4f7234](https://github.com/xuchengpeng/.emacs.d/commit/a4f723473b10b4dfd18ad61f8002713d82247494))
* **which-key:** set line-space in which-key buffer ([3e57da6](https://github.com/xuchengpeng/.emacs.d/commit/3e57da677381056196ac114afaf841ee7f70c5b1))

## [2.0.4](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.3...v2.0.4) (2023-05-16)


### Bug Fixes

* **git:** add magit package ([408c924](https://github.com/xuchengpeng/.emacs.d/commit/408c924d4aebe1b2b63218f44facf272ff566c75))
* **lsp:** remove lsp-mode,lsp-ui,consult-lsp ([5c9cf29](https://github.com/xuchengpeng/.emacs.d/commit/5c9cf29af2e3d7ee8392f93d1135af072e0eeb47))
* **modeline:** add buffer info with project directory ([19f89fc](https://github.com/xuchengpeng/.emacs.d/commit/19f89fc439dbb070c1e615209cbb2b0653ab3096))
* **modeline:** buffer-local-value is obsolete ([4cab98f](https://github.com/xuchengpeng/.emacs.d/commit/4cab98f10782c32243a1a7ea50e2d01aef9eef1e))

## [2.0.3](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.2...v2.0.3) (2023-05-16)


### Bug Fixes

* **consult:** support search current and other directory ([1379542](https://github.com/xuchengpeng/.emacs.d/commit/13795421786a5eda0e07ffbc60d2bccba3850f75))
* **diagnostics:** fix warnings ([55f8a21](https://github.com/xuchengpeng/.emacs.d/commit/55f8a21bc5b0675686f6eadbd22f815410f27b1e))
* **flycheck:** bind flycheck-command-map with C-c f ([f53f660](https://github.com/xuchengpeng/.emacs.d/commit/f53f660b5df2f65c15cfae0d8e90804e32a3018b))
* **modeline:** add matches ([8be2528](https://github.com/xuchengpeng/.emacs.d/commit/8be2528076bc15b70445cf7b19ee4e0656538822))
* **modeline:** refactoring segments ([5ad058b](https://github.com/xuchengpeng/.emacs.d/commit/5ad058bffd8ef17f5dbe51eebe0b58d461fe5008))
* **projectile:** bind projectile-command-map with C-c p ([33eb4ce](https://github.com/xuchengpeng/.emacs.d/commit/33eb4cede1afdca1061b29a1e68a4cf5b2b298f0))
* **winum:** active with window-configuration-change-hook ([79943b8](https://github.com/xuchengpeng/.emacs.d/commit/79943b83e3182a953ce6c88264d844ba94dcfcb4))

## [2.0.2](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.1...v2.0.2) (2023-05-12)


### Bug Fixes

* **recentf:** enabled with find-file-hook ([2a61c4f](https://github.com/xuchengpeng/.emacs.d/commit/2a61c4f4be6e9f9f2797b3e94c8b29962425ffaf))

## [2.0.1](https://github.com/xuchengpeng/.emacs.d/compare/v2.0.0...v2.0.1) (2023-05-12)


### Bug Fixes

* **company:** add company-mode keymaps ([3b5d111](https://github.com/xuchengpeng/.emacs.d/commit/3b5d111967b76a9823131563399bb32390d4e6e8))
* **encoding:** set utf-8 as default ([9a9af01](https://github.com/xuchengpeng/.emacs.d/commit/9a9af01a9c2504542d4a988d288179c800e29f2e))
* **keybinds:** add buffer,file,project,search keybindings ([033402c](https://github.com/xuchengpeng/.emacs.d/commit/033402c2979626d952f078101a16626781846269))
* **straight:** clone straight.el on first startup ([6b5b885](https://github.com/xuchengpeng/.emacs.d/commit/6b5b8856b7fc0b0753aa757284f2d1d604f43ce7))
* **themes:** clone doom-themes to local repository ([5cd6e49](https://github.com/xuchengpeng/.emacs.d/commit/5cd6e49e4786df7426f2441b2dfb09a30c07aeae))

## [2.0.0](https://github.com/xuchengpeng/.emacs.d/compare/v1.8.1...v2.0.0) (2023-05-11)


### ⚠ BREAKING CHANGES

* **config:** refactoring framework

### Code Refactoring

* **config:** refactoring framework ([8204f5a](https://github.com/xuchengpeng/.emacs.d/commit/8204f5a963ecc0fc58e47a081f945bf9ab38f911))

## [1.8.1](https://github.com/xuchengpeng/.emacs.d/compare/v1.8.0...v1.8.1) (2023-03-10)


### Bug Fixes

* **doc:** workflow status ([2b1932d](https://github.com/xuchengpeng/.emacs.d/commit/2b1932d0277887e74dbbf9ad45d3bed452adcb81))

## [1.8.0](https://github.com/xuchengpeng/.emacs.d/compare/v1.7.1...v1.8.0) (2023-03-10)


### Features

* **ci:** auto release ([4cdb239](https://github.com/xuchengpeng/.emacs.d/commit/4cdb239653474c9c92a5df557860cddf7c0c1ea8))
