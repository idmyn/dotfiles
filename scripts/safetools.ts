#!/usr/bin/env -S deno run --quiet --allow-all

import {
  buildApplication,
  buildCommand,
  buildRouteMap,
  type CommandContext,
  run as runApp,
} from "npm:@stricli/core@^1.2.5";
import { z, ZodType } from "npm:zod@^4.3.5";
import $ from "jsr:@david/dax@0.44.2";
import process from "node:process";

const DEFAULT_PORT = 1997;

const envSchema = z.object({
  SAFETOOLS_MODE: z
    .enum(["client", "server"])
    .default("client")
    .describe("Execution mode: client or server"),
  SAFETOOLS_PORT: z
    .string()
    .optional()
    .transform((val) => (val ? Number(val) : DEFAULT_PORT))
    .pipe(z.number().int().positive().max(65535))
    .describe("Port number for the server"),
});

const env = envSchema.parse({
  SAFETOOLS_MODE: Deno.env.get("SAFETOOLS_MODE"),
  SAFETOOLS_PORT: Deno.env.get("SAFETOOLS_PORT"),
});

const MODE = env.SAFETOOLS_MODE;
const PORT = env.SAFETOOLS_PORT;

type Response = {
  status: "error";
  message: string;
} | {
  status: "ok";
  result: unknown;
};

// === Command Registry ===

const serverCommandRegistry: Record<
  string,
  (args: unknown) => Promise<Response>
> = {};

async function sendCommand(
  command: string,
  args: unknown,
): Promise<Response> {
  const message = { command, args };
  const res = await fetch(`http://localhost:${PORT}/`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(message),
  });
  return await res.json();
}

function defineCommand<TSchema extends ZodType>(
  { name, schema, server, client }: {
    name: string;
    schema: TSchema;
    server: (args: z.infer<TSchema>) => string | Promise<string>;
    client: (
      sendCommand: (args: z.infer<TSchema>) => Promise<Response>,
    ) => ReturnType<typeof buildCommand>;
  },
) {
  serverCommandRegistry[name] = async (args: unknown) => {
    try {
      const result = await server(schema.parse(args));
      return {
        status: "ok",
        result,
      };
    } catch (err) {
      return {
        status: "error",
        message: `Command execution failed. ${err}`,
      };
    }
  };
  const command = client((args) => sendCommand(name, args));
  return command;
}

// === Client Commands ===

const cowsay = defineCommand({
  name: "cowsay",
  schema: z.object({ text: z.string() }),
  server: async (args) => {
    const text = String(args.text || "Moo!");
    const output = await $`cowsay ${text}`.stdout("piped")
      .stderr("piped").noThrow();
    if (output.code !== 0) throw new Error(output.stderr);
    return output.stdout;
  },
  client: (sendCommand) =>
    buildCommand({
      async func(this: CommandContext, _flags: unknown, text: string) {
        const response = await sendCommand(
          { text },
        );
        if (response.status === "ok") {
          console.log(response.result);
        } else {
          console.error(response.message);
        }
      },
      parameters: {
        positional: {
          kind: "tuple",
          parameters: [
            {
              brief: "Text for the cow to say",
              parse: String,
              placeholder: "text",
            },
          ],
        },
      },
      docs: {
        brief: "Make the cow say something",
      },
    }),
});

// === Server ===

async function startServer() {
  console.log(`Server listening on port ${PORT}`);

  Deno.serve({ port: PORT }, async (req) => {
    if (req.method !== "POST") {
      return Response.json(
        { status: "error", message: "Method not allowed" } satisfies Response,
        { status: 405 },
      );
    }

    try {
      const message = z.object({ command: z.string(), args: z.unknown() })
        .parse(await req.json());

      console.log(`Received: ${JSON.stringify(message)}`);

      const serverFn = serverCommandRegistry[message.command];

      // Validate command exists
      if (!serverFn) {
        return Response.json(
          {
            status: "error",
            message: `Unknown command: ${message.command}`,
          } satisfies Response,
          { status: 400 },
        );
      }

      const response = await serverFn(message.args);

      return Response.json(response);
    } catch (err) {
      return Response.json(
        {
          status: "error",
          message: `Invalid JSON: ${err}`,
        } satisfies Response,
        { status: 400 },
      );
    }
  });

  // Keep server running
  await new Promise(() => {});
}

// === Application ===

if (MODE === "server") {
  await startServer();
} else {
  const root = buildRouteMap({
    routes: {
      cowsay,
    },
    docs: {
      brief: "a collection of tools",
    },
  });

  const app = buildApplication(root, {
    name: "safetools",
  });

  await runApp(app, process.argv.slice(2), { process });
}
