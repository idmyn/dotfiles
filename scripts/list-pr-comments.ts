#!/usr/bin/env -S deno run --quiet --allow-all

import $ from "jsr:@david/dax@0.44.2";

async function getRepoInfo(): Promise<
  { repoArg: string; owner: string; repo: string }
> {
  // Check if we're in a git repository
  const isGitRepo = await $`git rev-parse --git-dir`.noThrow().quiet()
    .then((r) => r.code === 0);

  let repoIdentifier: string;

  if (isGitRepo) {
    // In git repo, gh can auto-detect but we still need owner/repo for GraphQL
    repoIdentifier =
      (await $`gh repo view --json nameWithOwner --jq '.nameWithOwner'`.text())
        .trim();
  } else {
    // Not in git repo, get from jj
    const remoteListOutput = await $`jj git remote list`.text();
    let originUrl = "";

    for (const line of remoteListOutput.trim().split("\n")) {
      const [remoteName, remoteUrl] = line.split(/\s+/);
      if (remoteName === "origin") {
        originUrl = remoteUrl;
        break;
      }
    }

    if (!originUrl) {
      console.error('Error: No "origin" remote found in jj git remote list');
      Deno.exit(1);
    }

    // Parse GitHub URL (SSH or HTTPS)
    const sshMatch = originUrl.match(/git@github\.com:(.+?)(?:\.git)?$/);
    const httpsMatch = originUrl.match(
      /https:\/\/github\.com\/(.+?)(?:\.git)?$/,
    );

    if (sshMatch) {
      repoIdentifier = sshMatch[1];
    } else if (httpsMatch) {
      repoIdentifier = httpsMatch[1];
    } else {
      console.error(`Error: Could not parse GitHub URL: ${originUrl}`);
      Deno.exit(1);
    }
  }

  const [owner, repo] = repoIdentifier.split("/");
  const repoArg = `--repo=${repoIdentifier}`;

  return { repoArg, owner, repo };
}

async function run() {
  const { repoArg, owner, repo } = await getRepoInfo();

  const bookmarkName = (
    await $`jj bookmark list -r "closest_bookmark(@)" -T "json(self)" | jq -r '.name'`
      .text()
  )
    .trim();

  const prNumber = (
    await $`gh pr list ${repoArg} --head ${bookmarkName} --json number --jq '.[0].number'`
      .text()
  )
    .trim();

  const prComments = await getPrComments(prNumber, owner, repo);

  return console.log(prComments);
}
await run();

async function getPrComments(prNumber: string, owner: string, repo: string) {
  const result = await $`gh api graphql -f query='
       query($owner: String!, $repo: String!, $number: Int!) {
         repository(owner: $owner, name: $repo) {
           pullRequest(number: $number) {
             comments(first: 100) {
               nodes {
                 author { login }
                 body
                 createdAt
               }
             }
             reviewThreads(first: 100) {
               nodes {
                 id
                 isResolved
                 isCollapsed
                 isOutdated
                 comments(first: 100) {
                   nodes {
                     author { login }
                     body
                     path
                     line
                     createdAt
                     isMinimized
                     minimizedReason
                   }
                 }
               }
             }
           }
         }
       }
       ' -f owner=${owner} -f repo=${repo} -F number=${prNumber}`.text();
  return result;
}
