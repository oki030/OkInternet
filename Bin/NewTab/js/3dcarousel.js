var count = 0; 
var baseSpeed = 0.05; 
var radiusX = 400; 
var radiusY = 100; 
var centerX = 250; 
var centerY = 190;
var speed = 0.3;
var imageDivs = '';
var numberOfElements = 0;
var carousel = '';
var speedTest = '';


window.addEvent('domready', function(){
  
	carousel = $('carousel');
	speedTest = $('speedTest');

	imageDivs = carousel.getElementsByTagName("div"); 
	numberOfElements = imageDivs.length; 
	
	setInterval('startCarousel()',40);
});

function startCarousel(){
	
	for(i=0; i < numberOfElements; i++){
	
		angle = i * ( Math.PI * 2 ) / numberOfElements;
	
		imageDivsStyle = imageDivs[ i ].style; 
		imageDivsStyle.position='absolute'; 
		
		posX = ( Math.sin( count * ( baseSpeed * speed ) + angle )* radiusX + centerX );
		posY = ( Math.cos( count * ( baseSpeed * speed ) + angle )* radiusY + centerY );
		
		
		imageDivsStyle.left = posX+"px"; 
		imageDivsStyle.top = posY+"px"
		
		imageDivWidth = posY;
		imageDivZIndex = Math.round(imageDivWidth)+100;
		
		imageDivsStyle.width = imageDivWidth+'px';
		imageDivsStyle.zIndex = imageDivZIndex;
		imageDivs[i].style.filter = 'alpha(opacity=' + (80 + Math.cos( count * ( baseSpeed * speed ) + angle )*100) + ')';
		
		angle += speed;
	
	}
	
	count++
}