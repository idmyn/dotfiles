#!/usr/bin/env -S deno run --quiet --allow-all

import $ from "jsr:@david/dax@0.44.2";

async function run() {
  const bookmarkName = (
    await $`jj bookmark list -r "closest_bookmark(@)" -T "json(self)" | jq -r '.name'`
      .text()
  )
    .trim();

  const prNumber = (
    await $`gh pr list --head ${bookmarkName} --json number --jq '.[0].number'`
      .text()
  )
    .trim();

  const prComments = await getPrComments(prNumber);

  return console.log(prComments);
}
await run();

async function getPrComments(prNumber: string) {
  const [owner, repo] = await Promise.all([
    $`gh repo view --json owner --jq '.owner.login'`
      .text()
      .then((r) => r.trim()),
    $`gh repo view --json name --jq '.name'`
      .text()
      .then((r) => r.trim()),
  ]);
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
             reviews(first: 100) {
               nodes {
                 author { login }
                 body
                 state
                 createdAt
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
