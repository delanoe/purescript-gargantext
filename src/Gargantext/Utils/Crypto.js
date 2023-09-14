import { subtle } from '../../src/external-deps/crypto.js';

async function getSHA256Hash(input) {
  const textAsBuffer = new TextEncoder().encode(input);
  const hashBuffer = await subtle.digest("SHA-256", textAsBuffer);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  const hash = hashArray
        .map((item) => item.toString(16).padStart(2, "0"))
        .join("");
  return hash;
};

export const getSHA256HashImpl = (s) => () =>
  getSHA256Hash(s);
