#section FRAGMENT_SHADER
uniform float density = 1.0;
uniform float weight = 0.01;
uniform float decay = 1.0;
uniform float exposure = 1.2;
uniform int samples = 100;
uniform vec2 origin = vec2(0.5, 0.5);
uniform sampler2D black_render_pass;
uniform sampler2D previous_pass;

in vec2 uv;
out vec4 color;

void main(){
  vec4 black_color = vec4(0.0, 0.0, 0.0, 0.0);
  vec2 delta_coord = vec2(uv - origin.xy);

  vec2 _uv = uv.xy;
  delta_coord *= (1.0 /  float(samples)) * density;
  float illumination_decay = 1.0;

  for(int i=0; i < samples; i++){
    _uv -= delta_coord;
    vec4 samp = texture(black_render_pass, _uv);
    samp *= illumination_decay * weight;
    black_color += samp;
    illumination_decay *= decay;
  }
  
  black_color *= exposure;
  color = texture(previous_pass, uv);
  color.xyz *= black_color.xyz;
  color.a += black_color.a;
}
