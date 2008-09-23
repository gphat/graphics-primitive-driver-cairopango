package Graphics::Primitive::Driver::CairoPango;
use Moose;
use Moose::Util::TypeConstraints;

use Cairo;
use Carp;
use Geometry::Primitive::Point;
use Geometry::Primitive::Rectangle;
use Graphics::Primitive::Driver::CairoPango::TextLayout;
use Gtk2;
use Gtk2::Pango;
use IO::File;
use Math::Trig ':pi';

with 'Graphics::Primitive::Driver';

our $AUTHORITY = 'cpan:GPHAT';
our $VERSION = '0.2';

enum 'Graphics::Primitive::Driver::CairoPango::Format' => (
    qw(PDF PS PNG SVG pdf ps png svg)
);

# If we encounter an operation with 'preserve' set to true we'll set this attr
# to the number of primitives in that path.  On each iteration we'll check
# this attribute.  If it's true, we'll skip that many primitives in the
# current path and then reset the value.  This allows us to leverage cairo's
# fill_preserve and stroke_perserve and avoid wasting time redrawing.
has '_preserve_count' => (
    isa => 'Str',
    is  => 'rw',
    default => sub { 0 }
);
has 'cairo' => (
    is => 'rw',
    isa => 'Cairo::Context',
    clearer => 'clear_cairo',
    lazy => 1,
    default => sub {
        my ($self) = @_;
        return Cairo::Context->create($self->surface);
    }
);
has 'format' => (
    is => 'ro',
    isa => 'Graphics::Primitive::Driver::CairoPango::Format',
    default => sub { 'PNG' }
);
has 'surface' => (
    is => 'rw',
    clearer => 'clear_surface',
    lazy => 1,
    default => sub {
        # Lazily create our surface based on the format they are required
        # to've chosen when creating this object
        my $self = shift;

        my $surface;

        my $width = $self->width;
        my $height = $self->height;

        if(uc($self->format) eq 'PNG') {
            $surface = Cairo::ImageSurface->create(
                'argb32', $width, $height
            );
        } elsif(uc($self->format) eq 'PDF') {
            croak('Your Cairo does not have PostScript support!')
                unless Cairo::HAS_PDF_SURFACE;
            $surface = Cairo::PdfSurface->create_for_stream(
                sub { $self->{DATA} .= $_[1] }, $self, $width, $height
                # $self->can('append_surface_data'), $self, $width, $height
            );
        } elsif(uc($self->format) eq 'PS') {
            croak('Your Cairo does not have PostScript support!')
                unless Cairo::HAS_PS_SURFACE;
            $surface = Cairo::PsSurface->create_for_stream(
                sub { $self->{DATA} .= $_[1] }, $self, $width, $height
                # $self->can('append_surface_data'), $self, $width, $height
            );
        } elsif(uc($self->format) eq 'SVG') {
            croak('Your Cairo does not have SVG support!')
                unless Cairo::HAS_SVG_SURFACE;
            $surface = Cairo::SvgSurface->create_for_stream(
                sub { $self->{DATA} .= $_[1] }, $self, $width, $height
                # $self->can('append_surface_data'), $self, $width, $height
            );
        } else {
            croak("Unknown format '".$self->format."'");
        }
        return $surface;
    }
);

sub data {
    my ($self) = @_;

    my $cr = $self->cairo;

    if(uc($self->format) eq 'PNG') {
        my $buff;
        $self->surface->write_to_png_stream(sub {
            my ($closure, $data) = @_;
            $buff .= $data;
        });
        return $buff;
    }

    $cr->show_page;

    $cr = undef;
    $self->clear_cairo;
    $self->clear_surface;

    return $self->{DATA};
}

around('draw', sub {
    my ($cont, $class, $comp) = @_;

    my $cairo = $class->cairo;

    $cairo->save;

    $cairo->translate($comp->origin->x, $comp->origin->y);
    $cairo->rectangle(0, 0, $comp->width, $comp->height);
    $cairo->clip;

    $cont->($class, $comp);

    $cairo->restore;
});

sub write {
    my ($self, $file) = @_;

    my $fh = IO::File->new($file, 'w')
        or die("Unable to open '$file' for writing: $!");
    $fh->binmode;
    $fh->print($self->data);
    $fh->close;
}

sub _draw_component {
    my ($self, $comp) = @_;

    my $width = $comp->width;
    my $height = $comp->height;

    my $context = $self->cairo;

    if(defined($comp->background_color)) {
        $context->set_source_rgba($comp->background_color->as_array_with_alpha);
        $context->rectangle(0, 0, $width, $height);
        $context->fill;
    }

    if(defined($comp->border)) {

        my $border = $comp->border;

        if($border->homogeneous) {
            # Don't bother if there's no width
            if($border->top->width) {
                $self->_draw_simple_border($comp);
            }
        } else {
            $self->_draw_complex_border($comp);
        }
    }
}

sub _draw_complex_border {
    my ($self, $comp) = @_;

    my ($mt, $mr, $mb, $ml) = $comp->margins->as_array;

    my $context = $self->cairo;
    my $border = $comp->border;

    my $width = $comp->width;
    my $height = $comp->height;

    my $bt = $border->top;
    my $thalf = (defined($bt) && defined($bt->color))
        ? $bt->width / 2: 0;

    my $br = $border->right;
    my $rhalf = (defined($br) && defined($br->color))
        ? $br->width / 2: 0;

    my $bb = $border->bottom;
    my $bhalf = (defined($bb) && defined($bb->color))
        ? $bb->width / 2 : 0;

    my $bl = $border->left;
    my $lhalf = (defined($bl) && defined($bl->color))
        ? $bl->width / 2 : 0;

    if($thalf) {
        $context->move_to($ml, $mt + $thalf);
        $context->set_source_rgba($bt->color->as_array_with_alpha);

        $context->set_line_width($bt->width);
        $context->rel_line_to($width - $mr - $ml, 0);

        my $dash = $bt->dash_pattern;
        if(defined($dash) && scalar(@{ $dash })) {
            $context->set_dash(0, @{ $dash });
        }

        $context->stroke;

        $context->set_dash(0, []);
    }

    if($rhalf) {
        $context->move_to($width - $mr - $rhalf, $mt);
        $context->set_source_rgba($br->color->as_array_with_alpha);

        $context->set_line_width($br->width);
        $context->rel_line_to(0, $height - $mb);

        my $dash = $br->dash_pattern;
        if(defined($dash) && scalar(@{ $dash })) {
            $context->set_dash(0, @{ $dash });
        }

        $context->stroke;
        $context->set_dash(0, []);
    }

    if($bhalf) {
        $context->move_to($width - $mr, $height - $bhalf - $mb);
        $context->set_source_rgba($bb->color->as_array_with_alpha);

        $context->set_line_width($bb->width);
        $context->rel_line_to(-($width - $mb), 0);

        my $dash = $bb->dash_pattern;
        if(defined($dash) && scalar(@{ $dash })) {
            $context->set_dash(0, @{ $dash });
        }

        $context->stroke;
    }

    if($lhalf) {
        $context->move_to($ml + $lhalf, $mt);
        $context->set_source_rgba($bl->color->as_array_with_alpha);

        $context->set_line_width($bl->width);
        $context->rel_line_to(0, $height - $mb);

        my $dash = $bl->dash_pattern;
        if(defined($dash) && scalar(@{ $dash })) {
            $context->set_dash(0, @{ $dash });
        }

        $context->stroke;
        $context->set_dash(0, []);
    }
}

sub _draw_simple_border {
    my ($self, $comp) = @_;

    my $context = $self->cairo;

    my $border = $comp->border;
    my $top = $border->top;
    my $bswidth = $top->width;

    $context->set_source_rgba($top->color->as_array_with_alpha);

    my @margins = $comp->margins->as_array;

    $context->set_line_width($bswidth);
    $context->set_line_cap($top->line_cap);
    $context->set_line_join($top->line_join);

    $context->new_path;
    my $swhalf = $bswidth / 2;
    my $width = $comp->width;
    my $height = $comp->height;
    my $mx = $margins[3];
    my $my = $margins[1];

    my $dash = $top->dash_pattern;
    if(defined($dash) && scalar(@{ $dash })) {
        $context->set_dash(0, @{ $dash });
    }

    $context->rectangle(
        $margins[3] + $swhalf, $margins[0] + $swhalf,
        $width - $bswidth - $margins[3] - $margins[1],
        $height - $bswidth - $margins[2] - $margins[0]
    );
    $context->stroke;

    # Reset dashing
    $context->set_dash(0, []);
}

sub _draw_textbox {
    my ($self, $comp) = @_;

    return unless(defined($comp->text));

    $self->_draw_component($comp);

    my $bbox = $comp->inside_bounding_box;

    my $context = $self->cairo;

    $context->set_source_rgba($comp->color->as_array_with_alpha);

    my $origin = $bbox->origin;
    my $x = $origin->x;
    my $y = $origin->y;

	my $valign = $comp->vertical_alignment;
	if($valign eq 'center') {
		$context->move_to($x, $y + ($comp->height / 2) - ($comp->layout->height / 2));
	} elsif($valign eq 'bottom') {
    	$context->move_to($x, $y + $comp->height - ($comp->layout->height / 2));
	} else {
		$context->move_to($x, $y);
	}

    if(defined($comp->lines)) {
        foreach my $line (@{ $comp->lines }) {
            my ($ink, $log) = $line->get_pixel_extents;
            $context->rel_move_to($log->{x}, abs($log->{y}) - 1);
            Gtk2::Pango::Cairo::show_layout_line($context, $line);
            $context->rel_move_to(0, $log->{height} + $log->{y});
        }
    } else {
        # my $layout = $self->_make_layout($comp);
        my $layout = $comp->layout->_layout;
        Gtk2::Pango::Cairo::update_layout($context, $layout);
        Gtk2::Pango::Cairo::show_layout($context, $layout);
    }
}

sub get_textbox_layout {
    my ($self, $comp) = @_;

    my $tl = Graphics::Primitive::Driver::CairoPango::TextLayout->new(
        component => $comp,
    );
    unless(defined($comp->text)) {
        $tl->height(0);
        return $tl;
    }

    my $context = $self->cairo;

    my $font = $comp->font;

    my $fontmap = Gtk2::Pango::Cairo::FontMap->get_default;
    $fontmap->set_resolution(72);

    my $desc = Gtk2::Pango::FontDescription->new;
    $desc->set_family($font->family);
    $desc->set_variant($font->variant);
    $desc->set_style($font->slant);
    $desc->set_weight($font->weight);
    $desc->set_size(Gtk2::Pango::units_from_double($font->size));

    my $layout = Gtk2::Pango::Cairo::create_layout($context);

    $layout->set_font_description($desc);
    $layout->set_markup($comp->text);
    $layout->set_indent(Gtk2::Pango::units_from_double($comp->indent));
    $layout->set_alignment($comp->horizontal_alignment);
    $layout->set_justify($comp->justify);
    $layout->set_wrap($comp->wrap_mode);
    $layout->set_ellipsize($comp->ellipsize_mode);

    if(defined($comp->line_height)) {
        $layout->set_spacing(Gtk2::Pango::units_from_double($comp->line_height - $comp->font->size));
    }

    my $pcontext = $layout->get_context;

	my $width = $comp->width ? $comp->inside_width : $comp->minimum_inside_width;
	$width = -1 if(!defined($width) || ($width == 0));

    $layout->set_width(Gtk2::Pango::units_from_double($width));
    # if($bbox->height) {
    #     $layout->set_height(Gtk2::Pango::units_from_double($bbox->height));
    # }

    Gtk2::Pango::Cairo::update_layout($context, $layout);

    my ($ink, $log) = $layout->get_pixel_extents;

    # my $y = 0;
    # if($comp->vertical_alignment eq 'bottom') {
    #     $y = $height - $log->{height};
    # } elsif($comp->vertical_alignment eq 'center') {
    #     $y = $height2 - $log->{height} / 2;
    # }

    # Rotation, for the future
    # $context->translate($bbox->width / 2, $bbox->height / 2);
    # $context->rotate(-3.14 / 2);
    # $context->translate(-$bbox->width / 2, -$bbox->height / 2);

	$tl->_layout($layout);
	$tl->width($log->{width});
	$tl->height($log->{height});

	return $tl;
}


sub _draw_arc {
    my ($self, $arc) = @_;

    my $context = $self->cairo;
    my $o = $arc->origin;
    if($arc->angle_start > $arc->angle_end) {
        $context->arc_negative(
            $o->x, $o->y, $arc->radius, $arc->angle_start, $arc->angle_end
        );
    } else {
        $context->arc(
            $o->x, $o->y, $arc->radius, $arc->angle_start, $arc->angle_end
        );
    }
}

sub _draw_bezier {
    my ($self, $bezier) = @_;

    my $context = $self->cairo;
    my $start = $bezier->start;
    my $end = $bezier->end;
    my $c1 = $bezier->control1;
    my $c2 = $bezier->control2;

    $context->curve_to($c1->x, $c1->y, $c2->x, $c2->y, $end->x, $end->y);
}

sub _draw_canvas {
    my ($self, $comp) = @_;

    $self->_draw_component($comp);

    foreach (@{ $comp->paths }) {

        $self->_draw_path($_->{path}, $_->{op});
    }
}

sub _draw_circle {
    my ($self, $circle) = @_;

    my $context = $self->cairo;
    my $o = $circle->origin;
    $context->arc(
        $o->x, $o->y, $circle->radius, 0, pi2
    );
}

sub _draw_ellipse {
    my ($self, $ell) = @_;

    my $cairo = $self->cairo;
    my $o = $ell->origin;

    $cairo->new_sub_path;
    $cairo->save;
    $cairo->translate($o->x, $o->y);
    $cairo->scale($ell->width / 2, $ell->height / 2);
    $cairo->arc(
        0, 0, 1, 0, pi2
    );
    $cairo->restore;
}

sub _draw_image {
    my ($self, $comp) = @_;

    $self->_draw_component($comp);

    my $cairo = $self->cairo;

    $cairo->save;

    my $imgs = Cairo::ImageSurface->create_from_png($comp->image);

    my $bb = $comp->inside_bounding_box;

    my $bumpx = 0;
    my $bumpy = 0;
    if($comp->horizontal_alignment eq 'center') {
        $bumpx = $bb->width / 2;
        if(defined($comp->scale)) {
            $bumpx -= $comp->scale->[0] * ($imgs->get_width / 2);
        } else {
            $bumpx -= $imgs->get_width / 2;
        }
    } elsif($comp->horizontal_alignment eq 'right') {
        $bumpx = $bb->width;
        if(defined($comp->scale)) {
            $bumpx -= $comp->scale->[0] * $imgs->get_width;
        } else {
            $bumpx -= $imgs->get_width;
        }
    }

    if($comp->vertical_alignment eq 'center') {
        $bumpy = $bb->height / 2;
        if(defined($comp->scale)) {
            $bumpy -= $comp->scale->[1] * ($imgs->get_height / 2);
        } else {
            $bumpy -= $imgs->get_height / 2;
        }
    } elsif($comp->vertical_alignment eq 'bottom') {
        $bumpy = $bb->height;
        if(defined($comp->scale)) {
            $bumpy -= $comp->scale->[1] * $imgs->get_height;
        } else {
            $bumpy -= $imgs->get_height;
        }
    }

    $cairo->translate($bb->origin->x + $bumpx, $bb->origin->y + $bumpy);
    $cairo->rectangle(0, 0, $imgs->get_width, $imgs->get_width);
    $cairo->clip;

    if(defined($comp->scale)) {
        $cairo->scale($comp->scale->[0], $comp->scale->[1]);
    }

    $cairo->rectangle(
       0, 0, $imgs->get_width, $imgs->get_height
    );

    $cairo->set_source_surface($imgs, 0, 0);

    $cairo->fill;

    $cairo->restore;
}

sub _draw_path {
    my ($self, $path, $op) = @_;

    my $context = $self->cairo;

    # If preserve count is set we've "preserved" a path that's made up 
    # of X primitives.  Set the sentinel to the the count so we skip that
    # many primitives
    my $pc = $self->_preserve_count;
    if($pc) {
        $self->_preserve_count(0);
    } else {
        $context->new_path;
    }

    my $pcount = $path->primitive_count;
    for(my $i = $pc; $i < $pcount; $i++) {
        my $prim = $path->get_primitive($i);
        my $hints = $path->get_hint($i);

        if(defined($hints)) {
            unless($hints->{contiguous}) {
                my $ps = $prim->point_start;
                $context->move_to(
                    $ps->x, $ps->y
                );
            }
        }

        # FIXME Check::ISA
        if($prim->isa('Geometry::Primitive::Line')) {
            $self->_draw_line($prim);
        } elsif($prim->isa('Geometry::Primitive::Rectangle')) {
            $self->_draw_rectangle($prim);
        } elsif($prim->isa('Geometry::Primitive::Arc')) {
            $self->_draw_arc($prim);
        } elsif($prim->isa('Geometry::Primitive::Bezier')) {
            $self->_draw_bezier($prim);
        } elsif($prim->isa('Geometry::Primitive::Circle')) {
            $self->_draw_circle($prim);
        } elsif($prim->isa('Geometry::Primitive::Ellipse')) {
            $self->_draw_ellipse($prim);
        } elsif($prim->isa('Geometry::Primitive::Polygon')) {
            $self->_draw_polygon($prim);
        }
    }

    if($op->isa('Graphics::Primitive::Operation::Stroke')) {
        $self->_do_stroke($op);
    } elsif($op->isa('Graphics::Primitive::Operation::Fill')) {
        $self->_do_fill($op);
    }

    if($op->preserve) {
        $self->_preserve_count($path->primitive_count);
    }
}

sub _draw_line {
    my ($self, $line) = @_;

    my $context = $self->cairo;
    my $end = $line->end;
    $context->line_to($end->x, $end->y);
}

sub _draw_polygon {
    my ($self, $poly) = @_;

    my $context = $self->cairo;
    for(my $i = 1; $i < $poly->point_count; $i++) {
        my $p = $poly->get_point($i);
        $context->line_to($p->x, $p->y);
    }
    $context->close_path;
}

sub _draw_rectangle {
    my ($self, $rect) = @_;

    my $context = $self->cairo;
    $context->rectangle(
        $rect->origin->x, $rect->origin->y,
        $rect->width, $rect->height
    );
}

sub _do_fill {
    my ($self, $fill) = @_;

    my $context = $self->cairo;
    my $paint = $fill->paint;

    # FIXME Check::ISA?
    if($paint->isa('Graphics::Primitive::Paint::Gradient')) {

        if($paint->style eq 'linear') {
            my $patt = Cairo::LinearGradient->create(
                $paint->line->start->x, $paint->line->start->y,
                $paint->line->end->x, $paint->line->end->y,
            );
            foreach my $stop ($paint->stops) {
                my $color = $paint->get_stop($stop);
                $patt->add_color_stop_rgba(
                    $stop, $color->red, $color->green,
                    $color->blue, $color->alpha
                );
            }
            $context->set_source($patt);
        } elsif($paint->style eq 'radial') {
            # TODO
        } else {
            croak('Unknown gradient type: '.$paint->style);
        }
    } elsif($paint->isa('Graphics::Primitive::Paint::Solid')) {
        $context->set_source_rgba($paint->color->as_array_with_alpha);
    }

    if($fill->preserve) {
        $context->fill_preserve;
    } else {
        $context->fill;
    }
}

sub _do_stroke {
    my ($self, $stroke) = @_;

    my $br = $stroke->brush;

    my $context = $self->cairo;
    $context->set_source_rgba($br->color->as_array_with_alpha);
    $context->set_line_cap($br->line_cap);
    $context->set_line_join($br->line_join);
    $context->set_line_width($br->width);

    my $dash = $br->dash_pattern;
    if(defined($dash) && scalar(@{ $dash })) {
        $context->set_dash(0, @{ $dash });
    }

    if($stroke->preserve) {
        $context->stroke_preserve;
    } else {
        $context->stroke;
    }

    # Reset dashing
    $context->set_dash(0, []);
}

sub _finish_page {
    my ($self) = @_;

    my $context = $self->cairo;
    $context->show_page;
}

sub _resize {
    my ($self, $width, $height) = @_;

    # Don't resize unless we have to
    if(($self->width != $width) || ($self->height != $height)) {
        $self->surface->set_size($width, $height);
    }
}

sub reset {
    my ($self) = @_;

    $self->clear_cairo;
}

no Moose;
1;
__END__

=head1 NAME

Graphics::Primitive::Driver::CairoPango - Cairo/Pango backend for Graphics::Primitive

=head1 SYNOPSIS

    use Graphics::Pritive::Component;
    use Graphics::Pritive::Component;
    use Graphics::Primitive::Driver::CairoPango;

    my $driver = Graphics::Primitive::Driver::CairoPango->new;
    my $container = Graphics::Primitive::Container->new(
        width => $form->sheet_width,
        height => $form->sheet_height
    );
    $container->border->width(1);
    $container->border->color($black);
    $container->padding(
        Graphics::Primitive::Insets->new(top => 5, bottom => 5, left => 5, right => 5)
    );
    my $comp = Graphics::Primitive::Component->new;
    $comp->background_color($black);
    $container->add_component($comp, 'c');

    my $lm = Layout::Manager::Compass->new;
    $lm->do_layout($container);

    my $driver = Graphics::Primitive::Driver::CairoPango->new(
        format => 'PDF'
    );
    $driver->draw($container);
    $driver->write('/Users/gphat/foo.pdf');

=head1 DESCRIPTION

This module draws Graphics::Primitive objects using Cairo and Pango.  This
is a seperate distribution because the current Pango bindings require
GTK+.  The Pango specific bits will be rolled into the normal Cairo driver
when this changes.

=head1 IMPLEMENTATION DETAILS

=over 4

=item B<Borders>

Borders are drawn clockwise starting with the top one.  Since cairo can't do
line-joins on different colored lines, each border overlaps those before it.
This is not the way I'd like it to work, but i'm opting to fix this later.
Consider yourself warned.

=back

=head1 METHODS

=head2 Constructor

=over 4

=item I<new>

Creates a new Graphics::Primitive::Driver::CairoPango object.  Requires a format.

  my $driver = Graphics::Primitive::Driver::CairoPango->new(format => 'PDF');

=back

=head2 Instance Methods

=over 4

=item I<cairo>

This driver's Cairo::Context object

=item I<data>

Get the data in a scalar for this driver.

=item I<draw>

Draws the specified component.  Container's components are drawn recursively.

=item I<format>

Get the format for this driver.

=item I<get_text_bounding_box ($font, $text, $angle)>

Returns two L<Rectangles|Graphics::Primitive::Rectangle> that encloses the
supplied text. The origin's x and y maybe negative, meaning that the glyphs in
the text extending left of x or above y.

The first rectangle is the bounding box required for a container that wants to
contain the text.  The second box is only useful if an optional angle is
provided.  This second rectangle is the bounding box of the un-rotated text
that allows for a controlled rotation.  If no angle is supplied then the
two rectangles are actually the same object.

If the optional angle is supplied the text will be rotated by the supplied
amount in radians.

=item I<get_textbox_layout ($font, $textbox)>

Returns this driver's implementation of a
L<TextLayout|Graphics::Primitive::Driver::TextLayout>.

=item I<reset>

Reset the driver.

=item I<surface>

Get/Set the surface on which this driver is operating.

=item I<write>

Write this driver's data to the specified file.

=back

=head1 AUTHOR

Cory Watson, C<< <gphat@cpan.org> >>

Infinity Interactive, L<http://www.iinteractive.com>

=head1 BUGS

Please report any bugs or feature requests to C<bug-geometry-primitive at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Geometry-Primitive>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 COPYRIGHT & LICENSE

Copyright 2008 by Infinity Interactive, Inc.

L<http://www.iinteractive.com>

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.
