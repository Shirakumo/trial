const mat3 aces_input_mat = mat3(0.59719, 0.35458, 0.04823,
                                 0.07600, 0.90834, 0.01566,
                                 0.02840, 0.13383, 0.83777);

const mat3 aces_output_mat = mat3( 1.60475, -0.53108, -0.07367,
                                  -0.10208,  1.10813, -0.00605,
                                  -0.00327, -0.07276,  1.07602);

vec3 rrt_and_odt_fit(vec3 v){
  vec3 a = v*(v+0.0245786) - 0.000090537;
  vec3 b = v*(0.983729*v+0.4329510) + 0.238081;
  return a / b;
}

vec3 tone_map(vec3 color){
  color = aces_input_mat * color;
  color = rrt_and_odt_fit(color);
  return aces_output_mat * color;
}
