hi

this is a snaphsot of my old svn. i have no idea if this even compiles.

as the perfectstorm readme-file states, there are several
dependencies, which i have included here. toolkbox and vektor were
made by me and the cl-opengl and cl-cairo bindings include patches
made by me, which in the case of cl-opengl are now in the upstream
repo.

other dependencies are also listed in the perfectstorm readme.

concerning the state of the code:
at the time of the project freeze (i.e. me being distracted by shiny
computer graphics things) there was a major rewrite of the texture
generation section (cairo.lisp), so that one may be completely broken.

the rest ist in the usual state of a 5000-lines lisp program that was
the authors' first attempt at a larger lisp project.

interesting starting points may be main.lisp (obviously), opengl.lisp
and entities-classes.lisp.
units.lisp is more of a configuration file for the various units
and weapons.

if anyone wants to revive the project, i'd be excited. but at the time
i can't contribute.



perfectstorm was made by cupe (cupe@erleuchtet.org) and syb.
it is hereby released under the creative commons attribution license.
