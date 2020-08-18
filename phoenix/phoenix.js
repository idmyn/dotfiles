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
const windowToFull = new Key('f', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.full(currentScreen()))
})

const windowToLeft = new Key('j', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.left(currentScreen()))
})

const windowToRight = new Key(';', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.right(currentScreen()))
})

const windowToFullNextScreen = new Key('o', ['alt', 'ctrl'], () => {
  Window.focused().setFrame(windowLocations.full(nextScreen()))
})

const showOrOpenEmacs = new Key('e', ['alt', 'ctrl'], () => {
  if (App.get('Emacs')) {
    App.get('Emacs').focus()
  } else {
    App.launch('Emacs')
    setTimeout(() => App.get('Emacs').focus(), 100)
  }
})

const showOrOpenBrowser = new Key('w', ['alt', 'ctrl'], () => {
  if (App.get('Safari')) {
    App.get('Safari').focus()
  } else if (App.get('Firefox Developer Edition')) {
    App.get('Firefox Developer Edition').focus()
  } else if (App.get('Firefox')) {
    App.get('Firefox').focus()
  } else if (App.get('Brave Browser')) {
    App.get('Brave Browser').focus()
  } else {
    App.launch('Safari')
    setTimeout(() => App.get('Safari').focus(), 500)
  }
})

const showOrOpenTerminal = new Key('s', ['alt', 'ctrl'], () => {
  if (App.get('kitty') && App.get('kitty').windows().length) {
    App.get('kitty').focus()
  } else {
    Task.run('/bin/sh', ['-c', 'open -a kitty'])
    // not sure why "App.launch('kitty')" doesn't work
  }
})

const showOrOpenMail = new Key('m', ['alt', 'ctrl'], () => {
  if (App.get('Microsoft Outlook')) {
    App.get('Microsoft Outlook').focus()
  } else {
    App.launch('Microsoft Outlook')
    setTimeout(() => App.get('Microsoft Outlook').focus(), 100)
  }
})

const showOrOpenDash = new Key('d', ['alt', 'ctrl'], () => {
  if (App.get('Dash')) {
    App.get('Dash').focus()
  } else {
    App.launch('Dash')
  }
})

const showOrOpenNotes = new Key('n', ['alt', 'ctrl'], () => {
  if (App.get('WorkFlowy')) {
    App.get('WorkFlowy').focus()
  } else {
    App.launch('WorkFlowy')
  }
})

const showOrOpenSlack = new Key('c', ['alt', 'ctrl'], () => {
  if (App.get('Slack')) {
    App.get('Slack').focus()
  } else {
    App.launch('Slack')
  }
})

Phoenix.log("App.all", App.all().map(app => app.name()).filter(appName => /Brave Browser/gi.test(appName)))
/* eslint-enable no-unused-vars */
