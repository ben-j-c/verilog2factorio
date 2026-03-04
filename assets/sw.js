var cacheName = 'v2f-pwa';
var filesToCache = [
  './',
  './sw.js',
  './index.html',
  './v2f.js',
  './v2f_bg.wasm',
];

self.addEventListener('install', function (e) {
  e.waitUntil(
    caches.open(cacheName).then(function (cache) {
      return cache.addAll(filesToCache);
    })
  );
});

self.addEventListener('fetch', function (e) {
  e.respondWith(
    caches.match(e.request).then(function (response) {
      return response || fetch(e.request);
    })
  );
});
