/* global Screen, Window, App, Key */

// inspiration:
// https://github.com/Jaredk3nt/phoenix-padding
// https://github.com/mafredri/phoenix-config/blob/5f4d7083d3e6dbc8956e63c2e010a05333e3af24/src/phoenix.ts#L29-L47

const currentScreen = () => Screen.main().flippedVisibleFrame()

const nextScreen = () => {
  const index =
    Screen.all().indexOf(Screen.main()) + 1 === Screen.all().length
      ? 0
      : Screen.all().indexOf(Screen.main()) + 1
  return Screen.all()[index].flippedVisibleFrame()
}

const windowLocations = {
  full: (scr) => ({
    y: scr.y,
    x: scr.x,
    width: scr.width,
    height: scr.height
  }),
  left: (scr) => ({
    y: scr.y,
    x: scr.x,
    width: scr.width / 2,
    height: scr.height
  }),
  right: (scr) => ({
    y: scr.y,
    x: scr.x + scr.width / 2,
    width: scr.width / 2,
    height: scr.height
  })
}

/* eslint-disable no-unused-vars */
const windowToFull = new Key('f', ['cmd', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.full(currentScreen()))
})

const windowToLeft = new Key('j', ['cmd', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.left(currentScreen()))
})

const windowToRight = new Key(';', ['cmd', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.right(currentScreen()))
})

const windowToFullNextScreen = new Key('o', ['cmd', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.full(nextScreen()))
})

const showOrOpenEmacs = new Key('e', ['cmd', 'ctrl'], () => {
  if (App.get('Emacs')) {
    App.get('Emacs').focus()
  } else {
    App.launch('Emacs')
    setTimeout(() => App.get('Emacs').focus(), 100)
  }
})

const showOrOpenBrowser = new Key('w', ['cmd', 'ctrl'], () => {
  if (App.get('Safari')) {
    App.get('Safari').focus()
  } else if (App.get('Brave Browser')) {
    App.get('Brave Browser').focus()
  } else {
    App.launch('Safari')
    setTimeout(() => App.get('Safari').focus(), 500)
  }
})
/* eslint-enable no-unused-vars */
