in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;

void main(){
  color = texture(previous_pass, uv);
  float avg = 0.2126*color.x + 0.7152*color.y + 0.0722*color.z;
  color = vec4(avg, avg, avg, color.w);
}
