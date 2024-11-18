#section FRAGMENT_SHADER
uniform float density = 1.0;
uniform float weight = 0.01;
uniform float decay = 1.0;
uniform float exposure = 1.2;
uniform int samples = 100;
uniform vec2 origin = vec2(0.5, 0.5);
uniform sampler2D depth_map;
uniform sampler2D color_map;

in vec2 uv;
out vec4 color;

void main(){
  vec2 _uv = uv.xy;
  vec2 duv = vec2(uv - origin.xy) * (1.0 /  float(samples)) * density;
  float illumination_decay = 1.0;
  float darkness = 0.0;
  for(int i=0; i < samples; i++){
    _uv -= duv;
    float depth = (texture(depth_map, _uv).r >= 1.0) ? 0.0 : 1.0;
    float opacity = texture(color_map, _uv).a;
    darkness += depth * illumination_decay * weight * opacity;
    illumination_decay *= decay;
  }
  
  darkness *= exposure;
  color = texture(color_map, uv);
  color.rgb *= darkness;
}
