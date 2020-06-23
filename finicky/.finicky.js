module.exports = {
  defaultBrowser: "Safari",
  handlers: [
    {
      match: /.*/,
      // Opens the first running browsers in the list. If none are running, the first one will be started.
      browser: ["Safari", "Firefox Developer Edition", "Firefox", "Brave Browser"]
    }
  ]
};
