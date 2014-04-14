cuatroporocho-theme
===================

The cuatro por ocho emacs color theme.

Color theme for Emacs, technically and color-wise initially based on zenburn,
but changed quite drastically from there, both WRT colors and implementation.

The color theme does currently contain colors for win32 graphics mode and win32 console mode,
both for dark and light mode, which are implemented by four color theme classes.

* *Note:* not yet complete, some faces are not yet defined.
  Anyway, programming faces are supported.
  
* *Note:* As zenburn, this still requires a special load:

  ```(progn (load-theme 'cuatroporocho t) (load "cuatroporocho-theme")).```
