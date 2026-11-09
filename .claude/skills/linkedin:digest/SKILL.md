---
name: linkedin:digest
description: Read the user's LinkedIn feed and email them a ranked list of what matters in it, with the day's main claim as the subject
argument-hint: "(no arguments: it reads the feed as it is now)"
allowed-tools: mcp__playwright__browser_navigate, mcp__playwright__browser_snapshot, mcp__playwright__browser_evaluate, mcp__playwright__browser_close, Read, Write, Edit, Bash(date *), Bash(.claude/scripts/digest_mail.py *)
---

Read the LinkedIn feed and mail a ranked list of what matters in it.
`<date>` is `date +%F`, and every file goes in `digest/linkedin/`.
Every run ends with `browser_close`, whichever step it stops at.

1. **Open the feed.** `browser_navigate` to `https://www.linkedin.com/feed/`.
   A login, authwall or checkpoint page means the saved login has expired:
   stop, and say the user runs
   `! ~/.claude/scripts/playwright_login.sh https://www.linkedin.com/login`.
2. **Read it.** `browser_evaluate` with the function in
   `.claude/skills/linkedin:digest/feed_read.js`, saved with `filename`
   `digest/linkedin/<date>-feed.json`. It scrolls the feed, drops the
   follow-suggestion boxes, and returns each post's text, comments
   included, and its link: the author's recent posts, since a post box
   carries no post ID. An `n` of 0 is a failed read: stop and say so.
3. **Rank it.** Read `<date>-feed.json` and `jobs/profile/profile.txt`.
   Comments are ignored, and a post marked Promoted is an ad. Every
   figure comes from a post as written. Write
   `digest/linkedin/<date>.json` (`digest_mail.py --help`):
   - **title** — the day's main claim as one sentence with a verb, not a
     topic.
   - **subtitle** — `<first name>'s LinkedIn feed, read <date> at <time>:
     <n> unique items, <posts> posts, <ads> ads; comments ignored`.
   - **items** — the consequential posts, most consequential first, at
     most ten: a headline that states the point, who posted it and when,
     what it says in two or three sentences, why it matters to the user
     by the profile (job leads, interview material, their own work), and
     the post's link from the read.
   - **skip** — what looks important and is not, each `<label>: <reason>`:
     ads posing as news, an empty hiring block, claims not verified.
   - **note** — whose figures they are, that the counts come from one
     scroll, so LinkedIn's ranking chose what showed, and that each link
     opens the author's recent posts, where the post sits near the top.
4. **Send.**
   ```bash
   .claude/scripts/digest_mail.py digest/linkedin/<date>.json \
     --stamp .claude/digest/linkedin.last
   ```
   A refusal is reported as it prints, and nothing is retried.
