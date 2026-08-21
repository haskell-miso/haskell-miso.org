// Verifies that every demo's displayed source (the `<name>Source` multiline
// string) is exactly the code that runs (between `-- >>> <name>` and
// `-- <<< <name>` markers) in shared/Site/Demos.hs.
//
//   bun scripts/check-demos.ts
import { readFileSync } from "fs";

const file = readFileSync(new URL("../shared/Site/Demos.hs", import.meta.url), "utf8");
const lines = file.split("\n");

function region(name: string): string {
  const start = lines.indexOf(`-- >>> ${name}`);
  const end = lines.indexOf(`-- <<< ${name}`);
  if (start < 0 || end < 0) throw new Error(`markers for ${name} not found`);
  return lines.slice(start + 1, end).join("\n").trimEnd();
}

// Re-implement GHC MultilineStrings: strip common indentation, drop the
// leading newline and trailing whitespace-only line, then unescape.
function multiline(name: string): string {
  const header = lines.findIndex((l) => l.startsWith(`${name}Source = """`));
  if (header < 0) throw new Error(`${name}Source not found`);
  let i = header + 1;
  const body: string[] = [];
  while (!lines[i].trim().startsWith('"""')) body.push(lines[i++]);
  const nonBlank = body.filter((l) => l.trim().length > 0);
  const indent = Math.min(...nonBlank.map((l) => l.match(/^ */)![0].length));
  return body
    .map((l) => (l.trim().length ? l.slice(indent) : ""))
    .join("\n")
    .replace(/\\"/g, '"')
    .replace(/\\\\/g, "\\")
    .trimEnd();
}

const names = [...file.matchAll(/^-- >>> (\w+)$/gm)].map((m) => m[1]);
let ok = true;
for (const name of names) {
  const a = region(name);
  const b = multiline(name);
  if (a !== b) {
    ok = false;
    console.error(`✗ ${name}: displayed source differs from executing code`);
    const al = a.split("\n"), bl = b.split("\n");
    for (let i = 0; i < Math.max(al.length, bl.length); i++) {
      if (al[i] !== bl[i]) { console.error(`  code   ${i + 1}: ${al[i]}\n  source ${i + 1}: ${bl[i]}`); break; }
    }
  } else {
    console.log(`✓ ${name}`);
  }
}
process.exit(ok ? 0 : 1);
