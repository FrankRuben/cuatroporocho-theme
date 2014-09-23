cuatroporocho-theme
===================

The cuatro por ocho emacs color theme.

Color theme for Emacs, technically and color-wise initially based on zenburn,
but changed quite drastically from there, both WRT colors and implementation.

The color theme does currently contain colors for linux and win32 graphics mode 
and linux and win32 console mode,
all for dark and light mode, and all implemented by four color theme classes.
The linux console mode does have a very limited color set and I'm usually not
using it, so that one is not nice and not complete.
Whether the beholder finds the other modes nice lies in his or her eyes...

* *Note:* not yet complete, some faces are not yet defined.
  Anyway, programming faces are supported.
  
* *Note:* As zenburn, this still requires a special load:

  ```(progn (load-theme 'cuatroporocho t) (load "cuatroporocho-theme")).```
  
Screenshots
-----------

- Light theme, use ```(defvar cuatroporocho-background-mode 'light)```
![Light](https://cloud.githubusercontent.com/assets/2405440/2696332/80afd2ea-c3e1-11e3-99c5-157169379453.png)

- Dark theme, use ```(defvar cuatroporocho-background-mode 'dark)```
![Dark](https://cloud.githubusercontent.com/assets/2405440/2696335/8d2d2bd0-c3e1-11e3-97d9-b232596c4ced.png)
