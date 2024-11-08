#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;

void main(){
  ivec2 size = textureSize(previous_pass, 0);
  float sobelSizeH = intensity / size.x;
  float sobelSizeV = intensity / size.y;
  vec3 top         = texture(previous_pass, vec2(uv.x, uv.y + sobelSizeV)).rgb;
  vec3 bottom      = texture(previous_pass, vec2(uv.x, uv.y - sobelSizeV)).rgb;
  vec3 left        = texture(previous_pass, vec2(uv.x - sobelSizeH, uv.y)).rgb;
  vec3 right       = texture(previous_pass, vec2(uv.x + sobelSizeH, uv.y)).rgb;
  vec3 topLeft     = texture(previous_pass, vec2(uv.x - sobelSizeH, uv.y + sobelSizeV)).rgb;
  vec3 topRight    = texture(previous_pass, vec2(uv.x + sobelSizeH, uv.y + sobelSizeV)).rgb;
  vec3 bottomLeft  = texture(previous_pass, vec2(uv.x - sobelSizeH, uv.y - sobelSizeV)).rgb;
  vec3 bottomRight = texture(previous_pass, vec2(uv.x + sobelSizeH, uv.y - sobelSizeV)).rgb;
  vec3 sx = -topLeft + -2 * left + -bottomLeft + topRight   + 2 * right  + bottomRight;
  vec3 sy = -topLeft + -2 * top  + -topRight   + bottomLeft + 2 * bottom + bottomRight;
  color = vec4(sqrt(sx * sx + sy * sy), 1.0);
}
