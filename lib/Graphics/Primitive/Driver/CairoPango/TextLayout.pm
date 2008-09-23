package Graphics::Primitive::Driver::CairoPango::TextLayout;
use Moose;

use Graphics::Primitive::TextBox;

with 'Graphics::Primitive::Driver::TextLayout';

use Gtk2::Pango;

has '_layout' => (
    is => 'rw',
    isa => 'Gtk2::Pango::Layout',
);

sub slice {
    my ($self, $offset, $size) = @_;

    my $lay = $self->_layout;
    my $comp = $self->component;

    unless(defined($size)) {
        # If there was no size, give them the whole shebang.

        my $clone = $comp->clone;
        $clone->layout($self);
        $clone->minimum_width($self->width + $comp->inside_width);
        $clone->minimum_height($self->height + $comp->inside_height);

        return $clone;
    }

    my $lc = $lay->get_line_count;

    my $found = 0;
    my $using = 0;
    my @lines;
    for(my $i = 0; $i < $lc; $i++) {
        my $line = $lay->get_line($i);
        my ($ink, $log) = $line->get_pixel_extents;

        my $lh = $log->{height};
        last if (($lh + $using) > $size);
        if($found > $offset) {
            push(@lines, $line);
            $using += $lh;
        }
        $found += $lh;
    }

    return Graphics::Primitive::TextBox->new(
        layout => $self,
        minimum_width => $self->width,
        minimum_height => $using
    );
}

no Moose;
1;
__END__
=head1 NAME

Graphics::Primitive::Driver::CairoPango::TextLayout - Text layout engine

=head1 SYNOPSIS

    my $tl = $driver->get_textbox_layout($comp);
    ...

=head1 DESCRIPTION

Implements L<Graphics::Primitive::Driver::TextLayout>.  Please refer to it's
documentation for usage.

=head1 IMPLEMENTATION

This text layout engine uses Pango to layout text.

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