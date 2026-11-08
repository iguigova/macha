async () => {
  // A post box holds no post ID, so `link` is its author's recent activity,
  // where the post sits near the top; `key` is the card's componentkey.
  const out = new Map();
  let suggestions = 0;
  const seenSuggestions = new Set();
  const activity = (href) => {
    const m = href.match(/^https:\/\/www\.linkedin\.com\/(in|company)\/([^/?#]+)/);
    if (!m) return '';
    return m[1] === 'in'
      ? `https://www.linkedin.com/in/${m[2]}/recent-activity/all/`
      : `https://www.linkedin.com/company/${m[2]}/posts/`;
  };
  const grab = () => {
    for (const h of document.querySelectorAll('h2')) {
      if (h.innerText.trim() !== 'Feed post') continue;
      const box = h.closest('[role="listitem"]') || h.closest('li') || h.parentElement;
      const key = box.getAttribute('componentkey') || box.innerText.slice(0, 200);
      if (box.querySelector('a[href*="/mynetwork/discover-hub/"]')) {
        if (!seenSuggestions.has(key)) { seenSuggestions.add(key); suggestions++; }
        continue;
      }
      if (out.has(key)) continue;
      const text = box.innerText.replace(/\n{2,}/g, '\n').trim();
      // Line 1 is "Feed post"; line 2 is the author, or a context line naming
      // who liked, reposted or follows it, with the author on line 3.
      const lines = text.split('\n').map(s => s.trim());
      const context = /^Followed by |\b(likes|loves|reposted|commented on|celebrates|supports|finds) this\b|\bfollows?\b/;
      const author = (context.test(lines[1] || '') ? lines[2] : lines[1]) || '';
      const anchors = [...box.querySelectorAll('a[href]')].filter(a => activity(a.href));
      const own = anchors.find(a => author && a.innerText.trim().startsWith(author));
      const link = own ? activity(own.href) : '';
      out.set(key, { link, text: text.slice(0, 2500) });
    }
  };
  for (let i = 0; i < 60; i++) {
    grab();
    const more = [...document.querySelectorAll('button')]
      .find(b => /show more (feed updates|results)|new posts/i.test(b.innerText));
    if (more) more.click();
    window.scrollTo(0, document.body.scrollHeight);
    await new Promise(r => setTimeout(r, 1800));
  }
  grab();
  return { url: location.href, n: out.size, suggestions_skipped: suggestions, posts: [...out.values()] };
}
