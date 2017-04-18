in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;
uniform float threshold = 1.0;

void main(){
  color = texture(previousPass, texCoord);
  float sum = color.x+color.y+color.z;
  if(sum >= threshold){
    color = vec4(0);
  }
}
