#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;

void main(){
  ivec2 size = textureSize(previous_pass, 0);
  float sobelSizeH = intensity / size.x;
  float sobelSizeV = intensity / size.y;
  vec4 top         = texture(previous_pass, vec2(uv.x, uv.y + sobelSizeV));
  vec4 bottom      = texture(previous_pass, vec2(uv.x, uv.y - sobelSizeV));
  vec4 left        = texture(previous_pass, vec2(uv.x - sobelSizeH, uv.y));
  vec4 right       = texture(previous_pass, vec2(uv.x + sobelSizeH, uv.y));
  vec4 topLeft     = texture(previous_pass, vec2(uv.x - sobelSizeH, uv.y + sobelSizeV));
  vec4 topRight    = texture(previous_pass, vec2(uv.x + sobelSizeH, uv.y + sobelSizeV));
  vec4 bottomLeft  = texture(previous_pass, vec2(uv.x - sobelSizeH, uv.y - sobelSizeV));
  vec4 bottomRight = texture(previous_pass, vec2(uv.x + sobelSizeH, uv.y - sobelSizeV));
  vec4 sx = -topLeft + -2 * left + -bottomLeft + topRight   + 2 * right  + bottomRight;
  vec4 sy = -topLeft + -2 * top  + -topRight   + bottomLeft + 2 * bottom + bottomRight;
  color = sqrt(sx * sx + sy * sy);
}
