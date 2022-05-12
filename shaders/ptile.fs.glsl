#version 460
flat in int g_tile_id;
flat in int g_params;
in vec2 g_tex_coords;

uniform vec2 screen_size;
//uniform int palette_offset;

layout(std140) uniform Graphics {
    uvec4 graphics[4096];
};
layout(std140) uniform Color {
    vec4 colors[256];
};

out vec4 out_color;

void main() {
    int scale = g_params & 0xFF;
    int tile_id = g_tile_id;
    ivec2 icoord = ivec2(g_tex_coords);
    int pal = (g_params & 0x0F00) >> 8;
    icoord = icoord * 8 / scale;
    if ((g_params & 0x2000) != 0) { icoord.y = 7 - icoord.y; }
    if ((g_params & 0x1000) != 0) { icoord.x = 7 - icoord.x; }

    uvec4 part1 = graphics[tile_id * 2 + 0];
    uvec4 part2 = graphics[tile_id * 2 + 1];
    
    uint lpart1 = part1[icoord.y / 2];
    uint lpart2 = part2[icoord.y / 2];

    int line1 = int(lpart1 >> ((icoord.y % 4) * 16));
    int line2 = int(lpart2 >> ((icoord.y % 4) * 16));

    int px = 0;
    px |= ((line1 >> ( 7-icoord.x)) & 0x1) << 0;
    px |= ((line1 >> (15-icoord.x)) & 0x1) << 1;
    px |= ((line2 >> ( 7-icoord.x)) & 0x1) << 2;
    px |= ((line2 >> (15-icoord.x)) & 0x1) << 3;

    /*
    // planar
    int index = icoord.y;
    uvec4 part = graphics[index / 4 + (tile_id&0x7FF) * 2];
    uint line = part[index & 0x3];
    int px = int(line >> ((icoord.x^1)*4)) & 0xF;
    */
    out_color = colors[px + (pal << 4)];// + palette_offset];

	int sel = (g_params >> 16) & 0x3;

	// TODO: debranch
    if (sel == 1) {
    	vec4 color_sel = vec4(0.5,0.5,1.0,1.0);
    	if (px == 0) { out_color = vec4(0,0,0,0); }
		out_color = mix(out_color, color_sel, 0.4);
		out_color.b += 0.4;
    } else if (sel == 2) {
    	vec4 color_sel = vec4(1.0,0.5,0.5,1.0);
    	if (px == 0) { out_color = vec4(0,0,0,0); }
		out_color = mix(out_color, color_sel, 0.4);
		out_color.r += 0.4;
    } else {
		if (px == 0) { discard; }
    }

}
