// @ts-check
// @jessie-check

const makePromise = (...args) => new Promise(...args);

/**
 * @param {string} url
 * @param {{ http: any }} powers
 * @returns {Promise<string>}
 */
export function curl(url, { http }) {
  return makePromise((resolve, reject) => {
    const req = http.get(url, (response) => {
      let str = '';
      // console.log('Response is ' + response.statusCode);
      response.on('data', (chunk) => {
        str += chunk;
      });
      response.on('end', () => resolve(str));
    });
    req.end();
    req.on('error', reject);
  });
}

/**
 * @param root0
 * @param root0.http
 * @returns { typeof fetch }
 */
export function nodeFetch({ http }) {
  /** @type { typeof fetch } */
  function fetch(url, options = {}) {
    return makePromise((resolve, reject) => {
      const { method = 'GET', body } = options;
      if (typeof url !== 'string') {
        throw Error('not implemented');
      }
      const req = http.request(url, { method }, (res) => {
        res.setEncoding('utf8');
        let content = '';
        res.on('data', (data) => {
          content += data;
        });

        res.on('end', () => {
          /** @type any */
          const response = {
            ok: res.statusCode && res.statusCode >= 200 && res.statusCode < 300,
            status: res.statusCode,
            statusText: res.statusMessage,
            text: () => content,
            json: () => JSON.parse(content),
          };
          resolve(response);
        });
      });
      if (body !== undefined) {
        req.write(body);
      }
      req.on('error', reject);
      req.end();
    });
  }

  return fetch;
}
