async () => {
  const out = new Map();
  const grab = () => {
    for (const h of document.querySelectorAll('h2')) {
      if (h.innerText.trim() !== 'Feed post') continue;
      const box = h.closest('li') || h.parentElement;
      const link = [...box.querySelectorAll('a[href*="/feed/update/"], a[href*="/posts/"]')]
        .map(a => a.href.split('?')[0])[0] || '';
      const text = box.innerText.replace(/\n{2,}/g, '\n').trim();
      const key = text.slice(0, 200);
      if (!out.has(key)) out.set(key, { link, text: text.slice(0, 2500) });
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
  return { url: location.href, n: out.size, posts: [...out.values()] };
}
