#section FRAGMENT_SHADER
out vec4 color;
in vec3 world_position;

uniform samplerCube environment_map;
const float sample_delta = 0.025;
const float PI = 3.14159265359;

void main(){
  vec3 normal = normalize(world_position);
  // Gramm-Schmidt coordinate system
  vec3 up = vec3(0.0, 1.0, 0.0);
  vec3 right = normalize(cross(up, normal));
  up = normalize(cross(normal, right));

  // Main sampling loop
  vec3 irradiance = vec3(0);
  float sample_count = 0.0;
  for(float phi = 0.0; phi < 2.0 * PI; phi += sample_delta){
    for(float theta = 0.0; theta < 0.5 * PI; theta += sample_delta){
      vec3 tangent_sample = vec3(sin(theta) * cos(phi),  sin(theta) * sin(phi), cos(theta));
      vec3 sample_vec = tangent_sample.x * right + tangent_sample.y * up + tangent_sample.z * normal;
      irradiance += texture(environment_map, sample_vec).rgb * cos(theta) * sin(theta);
      sample_count++;
    }
  }
  irradiance = PI * irradiance * (1.0 / float(sample_count));

  color = vec4(irradiance, 1);
}
