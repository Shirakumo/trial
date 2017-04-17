in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  color = texture(previousPass, texCoord);
  color = vec4(1.0-color.x, 1.0-color.y, 1.0-color.z, color.w);
}
