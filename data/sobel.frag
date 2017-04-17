in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;
uniform float intensity = 1.0;

void main(){
  ivec2 size = textureSize(previousPass, 0);
  float sobelSizeH = intensity / size.x;
  float sobelSizeV = intensity / size.y;
  vec4 top         = texture(previousPass, vec2(texCoord.x, texCoord.y + sobelSizeV));
  vec4 bottom      = texture(previousPass, vec2(texCoord.x, texCoord.y - sobelSizeV));
  vec4 left        = texture(previousPass, vec2(texCoord.x - sobelSizeH, texCoord.y));
  vec4 right       = texture(previousPass, vec2(texCoord.x + sobelSizeH, texCoord.y));
  vec4 topLeft     = texture(previousPass, vec2(texCoord.x - sobelSizeH, texCoord.y + sobelSizeV));
  vec4 topRight    = texture(previousPass, vec2(texCoord.x + sobelSizeH, texCoord.y + sobelSizeV));
  vec4 bottomLeft  = texture(previousPass, vec2(texCoord.x - sobelSizeH, texCoord.y - sobelSizeV));
  vec4 bottomRight = texture(previousPass, vec2(texCoord.x + sobelSizeH, texCoord.y - sobelSizeV));
  vec4 sx = -topLeft - 2 * left - bottomLeft + topRight   + 2 * right  + bottomRight;
  vec4 sy = -topLeft - 2 * top  - topRight   + bottomLeft + 2 * bottom + bottomRight;
  color = sqrt(sx * sx + sy * sy);
}
