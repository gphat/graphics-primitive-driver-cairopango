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

Document::Writer::TextLayout - Text layout engine

=head1 SYNOPSIS

    use Document::Writer;

    my $doc = Document::Writer->new(default_color => ...);
    my $p = $doc->next_page($width, $height);
    $p->add_text_to_page($driver, $font, $text);
    ...

=head1 METHODS

=over 4

=item I<font>

Set/Get this text layout's font.

=item I<height>

Get the height of this text layout.  Only useful after C<layout> has been
called.

=item I<lines>

Set/Get this text layout's 'lines'.  This is an arrayref of hashrefs, where
each hashref has the following members:

=over 4

=item B<box>

The bounding box for the text in this line.  This bounding box does not
take rotations into consideration.

=item B<cb>

The bounding box of required for a container that intends to contain the text
in this line.  

=item B<text>

The text in this line.

=back

This data structure is the meat of a TextLayout.  The multi-line, unwrapped
text is broken down into this datastructure based on the C<width> attribute.

=item I<slice ($offset, [$size])>

Given an offset and an optional size, returns C<n> lines from this layout
that come as close to C<$size> without exceeding it.  This method is provided
to allow incremental rendering of text.  For example, if you have a series
of containers 80 units high, you might write code like this:

  for(my $i = 0; $i < 3; $i++) {
      $lines = $layout->slice($i * 80, 80);
      # render the text
  }

=item I<text>

Set/Get the text to be laid out.

=item I<width>

Get/Set the width at which the text in this TextLayout should be wrapped.

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