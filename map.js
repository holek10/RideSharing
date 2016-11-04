var map = L.map('map').setView([51.099, 17.026], 13);
                      
L.tileLayer('https://api.mapbox.com/styles/v1/mapbox/streets-v9/tiles/256/{z}/{x}/{y}?access_token={accessToken}', {
	attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors,  <a 
href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://mapbox.com">Mapbox</a>',
	maxZoom: 18,
	accessToken: 'pk.eyJ1IjoiaG9sZWsiLCJhIjoiY2l0dG81ajB1MDAwNDJvcW14bWxsdWN3diJ9.Pe6D3DGCxZfZV7ZQp_-CTA'
}).addTo(map);

L.marker([51.099, 17.026]).addTo(map).bindPopup('Company Headquarter').openPopup();
