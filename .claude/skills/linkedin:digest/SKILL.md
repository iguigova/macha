---
name: linkedin:digest
description: Read the user's LinkedIn feed and email them a ranked list of what matters in it, with the day's main claim as the subject
argument-hint: "(no arguments: it reads the feed as it is now)"
allowed-tools: mcp__playwright__browser_navigate, mcp__playwright__browser_snapshot, mcp__playwright__browser_evaluate, mcp__playwright__browser_close, Read, Write, Bash(date *), Bash(.claude/scripts/digest_mail.py *)
---

Read the LinkedIn feed and mail a ranked list of what matters in it.
`<date>` is `date +%F`, and every file goes in `.claude/linkedin_digest/`.
Every run ends with `browser_close`, whichever step it stops at.

1. **Open the feed.** `browser_navigate` to `https://www.linkedin.com/feed/`.
   A login, authwall or checkpoint page means the saved login has expired:
   stop, and say the user runs
   `! ~/.claude/scripts/playwright_login.sh https://www.linkedin.com/login`.
2. **Read it.** `browser_evaluate` with the function in
   `.claude/skills/linkedin:digest/feed_read.js`, saved with `filename`
   `.claude/linkedin_digest/<date>-feed.json`. It scrolls the feed and
   returns each post's text and link, comments included in the text.
   An `n` of 0 is a failed read: stop and say so.
3. **Rank it.** Read the posts and `jobs/profile/profile.txt`. Comments
   are ignored; a post marked Promoted is an ad, and a follow suggestion
   is not a post. Every figure comes from a post as written. Write
   `.claude/linkedin_digest/<date>.json` in the shape
   `.claude/scripts/digest_mail.py --help` states:
   - **title** — the day's main claim as one sentence with a verb, not a
     topic.
   - **subtitle** — `<first name>'s LinkedIn feed, read <date> at <time>:
     <n> unique items, <posts> posts, <ads> ads; comments ignored`.
   - **items** — the consequential posts, most consequential first, at
     most ten: a headline that states the point, who posted it and when,
     what it says in two or three sentences, why it matters to the user
     by the profile (job leads, interview material, their own work), and
     its link when the read has one.
   - **skip** — what looks important and is not, each `<label>: <reason>`:
     ads posing as news, an empty hiring block, claims not verified.
   - **note** — whose figures they are, and that the counts come from one
     scroll, so LinkedIn's ranking chose what showed.
4. **Send.**
   ```bash
   .claude/scripts/digest_mail.py .claude/linkedin_digest/<date>.json \
     --stamp .claude/linkedin_digest.last
   ```
   A refusal is reported as it prints, and nothing is retried.
