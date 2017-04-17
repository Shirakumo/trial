in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;

void main(){
  color = texture(previousPass, texCoord);
  float avg = 0.2126*color.x + 0.7152*color.y + 0.0722*color.z;
  color = vec4(avg, avg, avg, color.w);
}
