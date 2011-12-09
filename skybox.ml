class skybox =
  object(this)

method private load_texture filename =
    let img = GdkPixbuf.from_file filename in
    let w = GdkPixbuf.get_width img
    and h = GdkPixbuf.get_height img
    and region = GdkPixbuf.get_pixels img in
    let n_channels = GdkPixbuf.get_n_channels img in
    let row = GdkPixbuf.get_rowstride img in

    assert(GdkPixbuf.get_bits_per_sample img = 8);
    assert(row = n_channels * w);

    let image = GlPix.create `ubyte ~format:`rgb ~width:w ~height:h in
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
        let pos = i*row + j*n_channels in
        let red = Gpointer.get_byte region ~pos in
        let green = Gpointer.get_byte region ~pos:(pos+1) in
        let blue = Gpointer.get_byte region ~pos:(pos+2) in
        Raw.sets (GlPix.to_raw image) ~pos:(3*(i*w+j))
          [| red; green; blue |]
      done
    done;

    image

val mutable sky = Obj.magic None
  (*
  Transforme l'image chargé en texture
  *)
method create_texture_from_image texture_filename =
    let texture = this#load_texture texture_filename in
      sky <- GlTex.gen_texture ();
      Gl.enable `texture_2d;
      GlTex.bind_texture `texture_2d sky;
      GlTex.parameter ~target:`texture_2d (`mag_filter `linear);
      GlTex.parameter ~target:`texture_2d (`min_filter `linear);
      GlTex.parameter ~target:`texture_2d (`wrap_s `repeat);
      GlTex.parameter ~target:`texture_2d (`wrap_t `repeat);
      GluMisc.build_2d_mipmaps texture;
      Gl.disable `texture_2d

method draw_skybox =
  (* on indique que l'on veut utiliser le Z-buffer pour supprimer
     les parties cachees. De plus, cull_face va accelerer ces
     suppressions en ne tenant pas compte des faces du cube dont seule
     la partie interne est orientee vers la camera. *)

    (* on indique que l'on va plaquer une texture *)
    Gl.enable `texture_2d;
    GlTex.env (`mode `decal);

    (* on recupere la texture des cubes *)
    GlTex.bind_texture ~target:`texture_2d sky;

    (* affichage du cube. Les GlTex.coord2 plaquent
    la texture sur chaque face *)
    GlDraw.begins  `quads;

    GlTex.coord2(0.25,0.9999); GlDraw.vertex3(-2000., -2000., -2000.);
    GlTex.coord2(0.5,0.999); GlDraw.vertex3( 2000., -2000., -2000.);
    GlTex.coord2(0.5,0.65); GlDraw.vertex3( 2000., 2000.,  -2000.);
    GlTex.coord2(0.25,0.65); GlDraw.vertex3(-2000., 2000.,  -2000.);

    (* le dessus *)
    GlTex.coord2(0.26,0.01); GlDraw.vertex3(-2000.,  -2000., 2000.);
    GlTex.coord2(0.26,0.33); GlDraw.vertex3(-2000.,  2000.,  2000.);
    GlTex.coord2(0.49,0.33); GlDraw.vertex3( 2000.,  2000.,  2000.);
    GlTex.coord2(0.49,0.01); GlDraw.vertex3( 2000.,  -2000., 2000.);

    (* le devant *)
    GlTex.coord2(0.9999,0.65); GlDraw.vertex3(-2000., -2000., -2000.);
    GlTex.coord2(0.99999,0.34); GlDraw.vertex3(-2000.,  -2000., 2000.);
    GlTex.coord2(0.75,0.34); GlDraw.vertex3( 2000.,  -2000., 2000.);
    GlTex.coord2(0.75,0.65); GlDraw.vertex3( 2000., -2000., -2000.);

    (* l'arriere *)
    GlTex.coord2(0.25,0.66); GlDraw.vertex3(-2000., 2000.,  -2000.);
    GlTex.coord2(0.5,0.66); GlDraw.vertex3( 2000., 2000.,  -2000.);
    GlTex.coord2(0.5,0.33); GlDraw.vertex3( 2000.,  2000.,  2000.);
    GlTex.coord2(0.25,0.33); GlDraw.vertex3(-2000.,  2000.,  2000.);

    (* le cote droit *)
    GlTex.coord2(0.75,0.65); GlDraw.vertex3( 2000., -2000., -2000.);
    GlTex.coord2(0.75,0.34); GlDraw.vertex3( 2000.,  -2000., 2000.);
    GlTex.coord2(0.5,0.34); GlDraw.vertex3( 2000.,  2000.,  2000.);
    GlTex.coord2(0.5,0.65); GlDraw.vertex3( 2000., 2000.,  -2000.);

    (* le cote gauche *)
    GlTex.coord2(0.001,0.65); GlDraw.vertex3(-2000., -2000., -2000.);
    GlTex.coord2(0.25,0.65); GlDraw.vertex3(-2000., 2000.,  -2000.);
    GlTex.coord2(0.25,0.34); GlDraw.vertex3(-2000.,  2000.,  2000.);
    GlTex.coord2(0.001,0.34); GlDraw.vertex3(-2000.,  -2000., 2000.);

    GlDraw.ends ();
    Gl.disable `texture_2d

end
