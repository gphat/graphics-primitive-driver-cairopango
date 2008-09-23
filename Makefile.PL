use inc::Module::Install;

name     'Graphics-Primitive-Driver-CairoPango';
all_from 'lib/Graphics/Primitive/Driver/CairoPango.pm';
author   'Cory G Watson <gphat@cpan.org>';

build_requires 'Test::More';

test_requires 'File::Spec';

requires 'Cairo' => '1.060';
requires 'Gtk2' => '1.183';
requires 'Moose' => '0.57';
requires 'IO::File';
requires 'Geometry::Primitive' => '0.12';
requires 'Graphics::Primitive' => '0.32';
requires 'Math::Complex' => '1.54';

WriteAll;

