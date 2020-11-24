module.exports = {
  defaultBrowser: "Safari",
  handlers: [
    {
      // Open meet.google.com urls in Google Chrome
      match:  /.*meet\.google\.com.*/,
      browser: "Google Chrome"
    },
    {
      match: "open.spotify.com*",
      browser: "Spotify"
    },
    {
      match: /.*/,
      // Opens the first running browsers in the list. If none are running, the first one will be started.
      browser: ["Safari", "Google Chrome", "Firefox Developer Edition", "Firefox"]
    },
  ]
};
