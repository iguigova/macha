// The page's live parts: the profile's work facts, found by a word from a
// posting, and the outlets the news digest reads. Both are read from the
// repo's own files, so the page shows what the skills read.

const PROFILE = 'jobs/profile/profile.txt';
const SOURCES = '.claude/skills/news:digest/sources.tsv';

const $ = id => document.getElementById(id);
const el = (tag, props = {}, ...children) => {
  const node = Object.assign(document.createElement(tag), props);
  node.append(...children);
  return node;
};
const text = async path => {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`${path} answered ${response.status}`);
  return response.text();
};

// A work fact is a line that starts with an employer: "At Flourish, …".
// Its employer is the words before the first comma, without the dates a
// line such as "At Vivonet (2007–2008), …" carries.
const workFacts = profile => profile.split('\n')
  .filter(line => /^At [^,]+, /.test(line))
  .map(line => ({ line, employer: line.slice(3, line.indexOf(',')).replace(/\s*\(.*\)$/, '') }));

// A word matches as a whole word, so "Go" finds "in Go" and not "go-live"
// or "Google".
const pattern = word => new RegExp(`(?<![\\w-])${word.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}(?![\\w-])`, 'i');

const marked = (line, re) => {
  const m = line.match(re);
  if (!m) return [line];
  return [line.slice(0, m.index), el('mark', {}, m[0]), line.slice(m.index + m[0].length)];
};

const plural = (n, one, many = `${one}s`) => `${n.toLocaleString('en')} ${n === 1 ? one : many}`;

const showFacts = facts => {
  const word = $('word').value.trim();
  const employers = new Set(facts.map(f => f.employer)).size;
  if (!word) {
    $('summary').textContent = `${plural(facts.length, 'work fact')} from ${plural(employers, 'employer')}.`;
    $('facts').replaceChildren();
    return;
  }
  const re = pattern(word);
  const found = facts.filter(f => re.test(f.line));
  const groups = Map.groupBy(found, f => f.employer);
  $('summary').textContent = found.length
    ? `${plural(found.length, 'fact')} of ${facts.length.toLocaleString('en')} mention “${word}”, from ${plural(groups.size, 'employer')}.`
    : `No work fact mentions “${word}”. In an application, that requirement is a gap.`;
  $('facts').replaceChildren(...[...groups].map(([employer, list]) =>
    el('article', { className: 'employer' },
      el('h3', {}, employer),
      el('ul', {}, ...list.map(f => el('li', {}, ...marked(f.line, re)))))));
};

// sources.tsv: comment lines, then a header row, then one outlet a row.
const outlets = tsv => {
  const [header, ...rows] = tsv.split('\n').filter(l => l && !l.startsWith('#')).map(l => l.split('\t'));
  return rows.map(r => Object.fromEntries(header.map((h, i) => [h, r[i] ?? ''])));
};

const showOutlets = list => {
  const countries = new Set(list.map(o => o.country)).size;
  const kinds = new Set(list.map(o => o.kind)).size;
  $('outlets-summary').textContent = `${plural(list.length, 'outlet')} in ${plural(countries, 'country', 'countries')}, of ${plural(kinds, 'kind')} of owner.`;
  $('outlets').replaceChildren(
    el('thead', {}, el('tr', {}, ...['Section', 'Outlet', 'Country', 'Owner'].map(h => el('th', { scope: 'col' }, h)))),
    el('tbody', {}, ...list.map(o => el('tr', {},
      el('td', {}, o.section), el('td', {}, o.outlet), el('td', {}, o.country), el('td', {}, o.kind)))));
};

const fail = (id, e) => { $(id).textContent = `It did not load: ${e.message}.`; };

text(PROFILE).then(profile => {
  const facts = workFacts(profile);
  $('word').addEventListener('input', () => showFacts(facts));
  $('find').addEventListener('submit', e => e.preventDefault());
  showFacts(facts);
}).catch(e => fail('summary', e));

text(SOURCES).then(tsv => showOutlets(outlets(tsv))).catch(e => fail('outlets-summary', e));
