export interface OffsetRange { start: number; end: number }

// Select complete identifiers that intersect the user's non-empty ranges.
export function selectedNames(text: string, ranges: OffsetRange[]): string[] {
  const selected = ranges.filter((range) => range.start < range.end);
  if (selected.length === 0) {
    return [];
  }
  const names = new Set<string>();
  const identifiers = /(?<![A-Za-z0-9_'])[a-z_][A-Za-z0-9_']*/g;
  for (const match of text.matchAll(identifiers)) {
    const start = match.index;
    const end = start + match[0].length;
    if (selected.some((range) => start < range.end && end > range.start)) {
      names.add(match[0]);
    }
  }
  return [...names];
}
