self.addEventListener('install', function(e) {
  e.waitUntil(
    caches.open('sudoku').then(function(cache) {
      return cache.addAll(['/', '/manifest.json', '/elm.js']);
    })
  );
});

self.addEventListener('fetch', function(event) {
  event.respondWith(
    caches.match(event.request).then(function(response) {
      return response || fetch(event.request);
    })
  );
});
