var cacheName = 'v2f-pwa';
var filesToCache = [
  './',
  './index.html',
  './v2f.js',
  './v2f_bg.wasm',
];

self.addEventListener('install', function (e) {
  e.waitUntil(
    caches.open(cacheName).then(function (cache) {
      // Only cache the "stable" entry points
      return cache.addAll(filesToCache);
    })
  );
});

self.addEventListener('fetch', function (e) {
  e.respondWith(
    caches.match(e.request).then(function (response) {
      return response || fetch(e.request).then(function (res) {
        return caches.open(cacheName).then(function (cache) {
          if (res.status === 200) {
            cache.put(e.request, res.clone());
          }
          return res;
        });
      });
    })
  );
});
