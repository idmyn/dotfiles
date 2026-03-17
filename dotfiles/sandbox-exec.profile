;; ---------------------------------------------------------------------------
;; Custom: Audio Playback
;; Allow CoreAudio mach services for notification sounds.
;; ---------------------------------------------------------------------------

(allow mach-lookup
    (global-name "com.apple.audio.audiohald")
    (global-name "com.apple.audio.AudioComponentRegistrar")
)

;; ---------------------------------------------------------------------------
;; Custom: Deny gh execution
;; Block GitHub CLI from running inside the sandbox.
;; ---------------------------------------------------------------------------

(deny process-exec
    (literal "@gh@/bin/gh")
)

;; ---------------------------------------------------------------------------
;; Custom: Nix Profile Binaries
;; Allow sandboxed agents to execute specific binaries from the Nix profile.
;; Add additional (literal ...) entries below to allow more tools.
;; ---------------------------------------------------------------------------

(allow process-exec
    (literal "@grafana-loki@/bin/logcli")
)

;; Allow reading the Nix store (immutable, read-only) and the symlink chain
;; from ~/.nix-profile so the shell can resolve Nix binaries by name on PATH.
(allow file-read*
    (subpath "/Users/david/.nix-profile")
    (subpath "/Users/david/.local/state/nix/profiles")
    (subpath "/nix/store")
)
