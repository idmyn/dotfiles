#!/usr/bin/env -S deno run --quiet --allow-all

import {
  buildApplication,
  buildCommand,
  buildRouteMap,
  CommandContext,
  run as runApp,
} from "npm:@stricli/core@^1.2.5";
import $ from "jsr:@david/dax@0.44.2";
import process from "node:process";

// adapted from https://github.com/anthropics/claude-code/blob/main/.devcontainer/Dockerfile
const DOCKERFILE = `
  FROM debian:bookworm-slim
  RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    less \
    git \
    ripgrep \
    jq \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

  ARG TZ
  ENV TZ="$TZ"

  # Create the agentbox user and group
  RUN groupadd -r agentbox && useradd -r -g agentbox agentbox

  # Create workspace and config directories and set permissions
  RUN mkdir -p /workspace /home/agentbox && \
    chown -R agentbox:agentbox /workspace /home/agentbox

  USER agentbox

  RUN curl https://mise.run | sh
  RUN echo 'eval "$(~/.local/bin/mise activate --shims bash)"' >> ~/.bashrc
`;

type BuildFlags = {
  "no-cache": boolean;
};

const build = buildCommand({
  async func(this: CommandContext, flags: BuildFlags) {
    await $`docker build ${$.rawArg(
      flags["no-cache"] ? "--no-cache" : "",
    )} -t agentbox:latest -`.stdinText(DOCKERFILE);
    await $`mkdir -p ~/.local/share/agentbox/home`;
  },
  parameters: {
    flags: {
      "no-cache": {
        kind: "boolean",
        withNegated: false,
        brief: "forwards --no-cache flag to docker build",
      },
    },
  },
  docs: {
    brief: "build docker image",
  },
});

type RunArgs = readonly string[];

const run = buildCommand<{}, RunArgs, CommandContext>({
  async func(this: CommandContext, _flags, ...args: string[]) {
    const cwd = Deno.cwd();
    const command = args.length > 0 ? args[0] : "/bin/bash";
    const commandArgs = args.slice(1);

    await $`docker run --rm -it
      --network=host
      -v ~/.local/share/agentbox/home:/home/agentbox
      -v ${cwd}:/workspace
      -w /workspace
      agentbox:latest
      ${command}
      ${commandArgs}`;
  },
  parameters: {
    positional: {
      kind: "array",
      parameter: {
        brief: "Command and arguments to run in the container",
        parse: String,
        placeholder: "args",
      },
    },
  },
  docs: {
    brief:
      "run a command in agentbox container with current directory mounted to /workspace",
  },
});

const root = buildRouteMap({
  routes: {
    build,
    run,
  },
  docs: {
    brief: "All available commands",
  },
});

const app = buildApplication(root, {
  name: "agentbox",
});

await runApp(app, process.argv.slice(2), { process });
