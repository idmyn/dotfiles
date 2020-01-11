/* global Screen, Window, App, Key */

// inspiration:
// https://github.com/Jaredk3nt/phoenix-padding
// https://github.com/mafredri/phoenix-config/blob/5f4d7083d3e6dbc8956e63c2e010a05333e3af24/src/phoenix.ts#L29-L47

const scr = Screen.main().flippedVisibleFrame()

const windowLocations = {
  full: {
    y: 0,
    x: 0,
    width: scr.width,
    height: scr.height
  },
  left: {
    y: 0,
    x: 0,
    width: scr.width / 2,
    height: scr.height
  },
  right: {
    y: 0,
    x: scr.width / 2,
    width: scr.width / 2,
    height: scr.height
  }
}

const nextScreen = () => {
  // work in progress
  const index =
    Screen.all().indexOf(Screen.main()) + 1 === Screen.all().length
      ? 0
      : Screen.all().indexOf(Screen.main()) + 1
  return Screen.all()[index].flippedVisibleFrame()
}

/* eslint-disable no-unused-vars */
const windowToFull = new Key('f', ['cmd', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.full)
})

const windowToLeft = new Key('j', ['cmd', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.left)
})

const windowToRight = new Key(';', ['cmd', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.right)
})

const showOrOpenEmacs = new Key('e', ['cmd', 'ctrl'], () => {
  if (App.get('Emacs').windows().length === 0) {
    App.launch('Emacs Client')
    // 'Emacs Client' as an App is a bit tricky because windows go to 'Emacs'
    setTimeout(() => Window.focused().setFrame(windowLocations.full), 600)
  } else {
    App.get('Emacs').focus()
  }
})

const showOrOpenBrowser = new Key('w', ['cmd', 'ctrl'], () => {
  if (App.get('Safari')) {
    App.get('Safari').focus()
  } else if (App.get('Brave Browser')) {
    App.get('Brave Browser').focus()
  } else {
    App.launch('Safari')
  }
})
/* eslint-enable no-unused-vars */
