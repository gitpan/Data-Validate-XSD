package Data::Validate::XSD;

use strict;

=head1 NAME

Data::Validate::XSD - Validate complex structures by definition

=head1 SYNOPSIS

  use Data::Validate::XSD;

  my $validator = Data::Validate::XSD->new( \%definition );

  my ($errors, $error) = $validator->validate( \%data );

  if($error) {
    carp Dumper($errors);
  }

=head1 DESCRIPTION
  
  Based on xsd and xml validation, this is an attempt to provide those functions
  without either xml or the hidous errors given out by modules like XPath.

  The idea behind the error reporting is that the errors can reflect the structure
  of the original structure replacing each variable with an error code and message.
  It is possible to work out a one dimention error reporting scheme too which I may
  work on next.

=head1 definitionS

  A definition is a hash containing information like an xml node containing children.

  An example definition and the kind of data it validates:

$definition = {

    root => [ { name => 'input', type => 'newuser' } ],

    simpleTypes => [
      confirm  => { base => 'id',   match => '/input/password' },
      rname    => { base => 'name', minLength => 1 },
      password => { base => 'id',   minLength => 6 },
    ],

    complexTypes => {
      newuser => [
        { name => 'username',     type => 'id' },
        { name => 'password',     type => 'password' },
        { name => 'confirm',      type => 'confirm' },
        { name => 'firstName',    type => 'rname' },
        { name => 'familyName',   type => 'name',  minOccurs => 0 },
        { name => 'nickName',     type => 'name',  minOccurs => 0 },
        { name => 'emailAddress', type => 'email', minOccurs => 0 },
      ],
    },
};

$data = {
    input => {
      username     => 'abcdef',
      password     => '1234567',
      confirm      => '1234567',
      firstName    => 'test',
      familyName   => 'user',
      nickName     => 'foobar',
      emailAddress => 'foo@bar.com',
    }
};

=head1 RESULTS

The first result you get is a structure the second is a boolean, the boolean explains the total stuctures pass or fail status.

The structure that is returned is almost a mirror structure of the input:

$error = 0;
$errors = {
    _input => 0,
    input => {
       username     => 0,
       password     => 0,
       confirm      => 0,
       firstName    => 0,
       familyName   => 0,
       nickName     => 0,
       emailAddress => 0,
    }
},

=cut

use Carp;
use Scalar::Util qw(looks_like_number);
our $VERSION = "1.02";

# Error codes
our $NOERROR             = 0x00;
our $INVALID_TYPE        = 0x01;
our $INVALID_PATTERN     = 0x02;
our $INVALID_MINLENGTH   = 0x03;
our $INVALID_MAXLENGTH   = 0x04;
our $INVALID_MATCH       = 0x05;
our $INVALID_VALUE       = 0x06;
our $INVALID_NODE        = 0x07;
our $INVALID_ENUMERATION = 0x08;
our $INVALID_MIN_RANGE   = 0x09;
our $INVALID_MAX_RANGE   = 0x0A;
our $INVALID_NUMBER      = 0x0B;
our $INVALID_COMPLEX     = 0x10;
our $INVALID_CHILDREN    = 0x20;
our $INVALID_EXIST       = 0x21;
our $INVALID_MIN_OCCURS  = 0x22;
our $INVALID_MAX_OCCURS  = 0x23;
our $CRITICAL            = 0x40;

our %complexTypes = ();

our %simpleTypes = (
    string     => { pattern => '.*' },
    integer    => { pattern => '[\-]{0,1}\d+' },
    double     => { pattern => '[0-9\-\.]*' },
    token      => { base    => 'string', pattern => '\w+' },
    boolean    => { pattern => '1|0|true|false' },
    email      => { pattern => '.+@.+\..+' },
    date       => { pattern => '\d\d\d\d-\d\d-\d\d' },
    'time'     => { pattern => '\d\d:\d\d' },
    datetime   => { pattern => '\d\d\d\d-\d\d-\d\d \d\d:\d\d' },
    percentage => { base => 'double', minInclusive => 0, maxInclusive => 100 },
  );

=head1 METHODS

=head2 $class->new( $definition, $debug )

 Create a new validation object, debug will cause
 All error codes to be replaced by error strings.

=cut
sub new {
  my ($class, $definition, $debug) = @_;

  my $self = bless { debug => $debug, strict => 1 }, $class;
  $self->setDefinition( $definition );

  # Set the error codes and messages.
  if($debug) {
    $NOERROR             = 0;
    $INVALID_TYPE        = 'Invalid Node Type (not used) ['.$INVALID_TYPE.']';
    $INVALID_PATTERN     = 'Invalid Pattern: Regex Pattern failed ['.$INVALID_PATTERN.']';
    $INVALID_MINLENGTH   = 'Invalid MinLength: Not enough nodes present ['.$INVALID_MINLENGTH.']';
    $INVALID_MAXLENGTH   = 'Invalid MaxLength: Too many nodes present ['.$INVALID_MAXLENGTH.']';
    $INVALID_MATCH       = 'Invalid Match: Node to Node match failed ['.$INVALID_MATCH.']';
    $INVALID_VALUE       = 'Invalid Value ['.$INVALID_VALUE.']';
    $INVALID_NODE        = 'Invalid Node: Required data does not exist for this node ['.$INVALID_NODE.']';
    $INVALID_ENUMERATION = 'Invalid Enum: Data not equal to any values supplied ['.$INVALID_ENUMERATION.']';
    $INVALID_MIN_RANGE   = 'Invalid Number: Less than allowable range ['.$INVALID_MIN_RANGE.']';
    $INVALID_MAX_RANGE   = 'Invalid Number: Greater than allowable range ['.$INVALID_MAX_RANGE.']';
    $INVALID_NUMBER      = 'Invalid Number: Data is not a real number ['.$INVALID_NUMBER.']';
    $INVALID_COMPLEX     = 'Invalid Complex Type: Failed to validate Complex Type ['.$INVALID_COMPLEX.']';
    $INVALID_CHILDREN    = 'Invalid Children: Failed to validate child node ['.$INVALID_CHILDREN.']';
    $INVALID_MIN_OCCURS  = 'Invalid Occurs: Minium number of occurances not met ['.$INVALID_MIN_OCCURS.']';
    $INVALID_MAX_OCCURS  = 'Invalid Occurs: Maxium number of occurances exceeded ['.$INVALID_MAX_OCCURS.']';
    $CRITICAL            = 'Critical Problem [QUIT] ['.$CRITICAL.']';
  }
  return $self;
}

=head2 $class->new_from_file( $path, $filename, $debug )

  Create a new definition from a stored file.

=cut
sub new_from_file {
  my ($class, $filename, $debug) = @_;

  if(-f $filename) {
    my $definition = $class->_load_file( $filename );
    return $class->new( $definition, $debug );
  }
  croak("Validation Error: Could not find Validate Configuration '$filename'");
}

=head2 $validator->validate( $data )

  Validate a set of data against this validator.
  Returns $errors and $error (see synopsys)

=cut
sub validate {
  my ($self, $data) = @_;
  my $def = $self->{'definition'};

  use Data::Dumper;

  if(defined($def->{'root'}) and defined($data)) {
    return $self->_validate_elements( definition => $def->{'root'}, data => $data );
  } else {
    croak("VAL Error: No root document definition") if not defined($def->{'root'});
    croak("VAL Error: No data provided")            if not defined($data);
  }
}

=head2 $validator->setStrict( $bool )

  Should missing data be considered an error.

=cut
sub setStrict {
  my ($self, $bool) = @_;
  $self->{'strict'} = $bool;
}

=head2 $validator->setDefinition( $definition )

  Set the validators definition, will load it (used internally too)

=cut
sub setDefinition {
  my ($self, $definition) = @_;
  $self->{'definition'} = $self->_load_definition( $definition );
}

=head2 $validator->_load_definition( $definition )

  Internal method for loading a definition into the validator

=cut
sub _load_definition
{
  my ($self, $definition) = @_;

  $definition->{'simpleTypes'} = { %simpleTypes, %{$definition->{'simpleTypes'} || {}} };
  $definition->{'complexTypes'} = { %complexTypes, %{$definition->{'complexTypes'} || {}} };

  if(defined($definition->{'include'})) {
    if(ref($definition->{'include'}) eq "ARRAY") {
      foreach my $include (@{$definition->{'include'}}) {

        my $def = ref($include) ? $self->_load_definition( $include ) : $self->_load_definition_from_file( $include );
        
        if(defined($def->{'simpleTypes'})) {
          $self->_push_hash($definition->{'simpleTypes'}, $def->{'simpleTypes'});
        }

        if(defined($def->{'complexTypes'})) {
          $self->_push_hash($definition->{'complexTypes'}, $def->{'complexTypes'});
        }
      }
    } else {
      croak("Validator Error: include format needs to be an Array []");
    }
  }
  return $definition;
}

=head2 $validator->_load_definition_from_file( $filename )

  Internal method for loading a definition from a file

=cut
sub _load_definition_from_file {
  my ($self, $filename) = @_;
  my $definition = $self->_load_file( $filename );
  return $self->_load_definition( $definition );
}

=head2 $validator->_validate_elements( %p )

  Internal method for validating a list of elements;
  p: definition, data, mode

=cut
sub _validate_elements
{
  my ($self, %p) = @_;

  my $definition = $p{'definition'};
  my $data       = $p{'data'};
  my $errors     = {};
  my $errorcount = 0;

  # This should be AND or OR and controls the logic flow of the data varify
  my $mode = $p{'mode'} || 'AND';
  
  if(not UNIVERSAL::isa($definition, 'ARRAY')) {
    croak("definition is not in the correct format: expected array");
  }

  foreach my $element (@{$definition}) {
    my $error = 0;

    # Element data check
    if(UNIVERSAL::isa($element, 'HASH')) {
      
      my $name = $element->{'name'};

      # Skip element if it's not defined
      if(not $name) {
        carp('Skipping element, no name defined') if $self->{'debug'};
        return;
      }

      # Data hold a node which does not exist in the definition
      if(not defined($data->{$name}) and $self->{'strict'}) {
        #carp "VALID DATA: ".join(', ', keys(%{$data}))."\n" if $self->{'debug'};
        $errors->{$name} = $INVALID_NODE;
        $error = $INVALID_CHILDREN;
        next;
      }
    
      $element->{'minOccurs'} = 1 if not defined($element->{'minOccurs'});
      $element->{'maxOccurs'} = 1 if not defined($element->{'maxOccurs'});
      $element->{'type'} = 'string' if not defined($element->{'type'});

      my ($te, $ne);
      ($ne, $error) = $self->_validate_element(
        definition => $element,
        data       => $data->{$name},
        name       => $name,
      );
      
      confess "Error from validate element, errors not defined!" if not defined $error;

      # Fill Errors with required results.
      if($error or $self->{'showall'}) {
        $errors->{$name} = $ne;
        if(ref($errors->{$name}) eq "HASH") {
          $errors->{"_".$name} = $error;
        }
      }

    } elsif(UNIVERSAL::isa($element, 'ARRAY')) {

      my $subr = {};
      my $newmode = $mode eq 'OR' ? 'AND' : 'OR';
      ($subr, $error) = $self->_validate_elements(
        definition => $element,
        data       => $data,
        mode       => $mode eq 'OR' ? 'AND' : 'OR',
      );
      map { $errors->{$_} = $subr->{$_} } keys(%{$subr});
    } else {
      carp "This is a complex type, but it doesn't look like one: $element";
    }

    if($error) {
      $errorcount++;
    } 
  }
  # Only invalidate parent if all elements have errored and
  # OR mode or any elements have errored and AND mode
  my $error = $INVALID_CHILDREN if
    ($mode eq 'OR' and $errorcount == @{$definition})
   or ($mode eq 'AND' and $errorcount > 0);
  $error = $NOERROR if not defined( $error );

  return $errors, $error;
}

=head2 $validator->_validate_element( %p )

  Internal method for validating a single element
  p: data, definition, mode

=cut
sub _validate_element {
  my ($self, %p) = @_;

  my $definition = $p{'definition'};
  my $data       = $p{'data'};
  my $name       = $p{'name'};

  my @results;
  my $proped = 0;
  my $error  = 0;

  if(ref($data) ne "ARRAY" and defined($data)) {
     $proped = 1;
     $data = [$data];
  }

  # minOccurs checking
  if($definition->{'minOccurs'} >= 1) {
    if(defined($data)) {
      if($definition->{'minOccurs'} > @{$data}) {
        return $INVALID_MIN_OCCURS, $CRITICAL;
      }
    } else {
      return $INVALID_EXIST, $CRITICAL;
    }
  }

  if(defined($data)) {
    # maxOccurs Checking
    if($definition->{'maxOccurs'} ne 'unbounded') {
      if($definition->{'maxOccurs'} < @{$data}) {
        return $INVALID_MAX_OCCURS, $CRITICAL;
      }
    }
    
    foreach my $element (@{$data}) {
      # fixed and default checking
      if(defined($definition->{'fixed'})) {
        if(ref($element) ne "") {
          croak("Validator Error: You can't check a fixed value on a complexType");
          return $CRITICAL;
        }
        if($element and $element ne $definition->{'fixed'}) {
          push @results, $INVALID_VALUE;
          $error = 1;
        }
      }

      if(defined($definition->{'default'})) {
        $element = $definition->{'default'} if not defined($element);
      }

      my %po;
      $po{'minLength'} = $definition->{'minLength'} if defined($definition->{'minLength'});
      $po{'maxLength'} = $definition->{'maxLength'} if defined($definition->{'maxLength'});

      # Element type checking
      my ($result, $te) = $self->_validate_type(
        type => $definition->{'type'},
        data => $element,
        %po, #Passable Options
      );

      push @results, $result;
      $error = $INVALID_CHILDREN if $te;
    }
  }

  if(@results > 0) {
    return ($proped ? $results[0] : \@results), $error;
  }
  return '', 0;
}

=head2 $validator->_validate_type( %p )

  Internal method for validating a single data type

=cut
sub _validate_type {
  my ($self, %p) = @_;

  my $data       = delete($p{'data'});
  my $type       = delete($p{'type'});
  my $definition = $self->{'definition'};
  my %pdef       = %p;

  if(defined($definition->{'simpleTypes'}->{$type})) {
    my $typedef = { %{$definition->{'simpleTypes'}->{$type}}, %pdef };
    # Base type check
    if(defined($typedef->{'base'})) {
      my ($err, $te) = $self->_validate_type(
        type => $typedef->{'base'},
        data => $data,
      );
      return $err, 1 if $te;
    }
    # Pattern type check
    if(defined($typedef->{'pattern'})) {
      my $pattern = $typedef->{'pattern'};
      
      if(not eval("\$data =~ /$pattern/")) {
        return $INVALID_PATTERN, 1;
      }
    }
    # Length checks
    if(defined($typedef->{'maxLength'})) {
      if(length($data) > $typedef->{'maxLength'}) {
        return $INVALID_MAXLENGTH, 1;
      }
    }
    if(defined($typedef->{'minLength'})) {
      if(length($data) < $typedef->{'minLength'}) {
        return $INVALID_MINLENGTH, 1;
      }
    }
    # Match another node
    if(defined($typedef->{'match'})) {
      if($data ne $self->_find_value( path => $typedef->{'match'}, data => $data )) {
        return $INVALID_MATCH, 1;
      }
    }
    if(defined($typedef->{'enumeration'})) {
      if(ref($typedef->{'enumeration'}) ne 'ARRAY') {
        croak("Validator Error: Enumberation not of the correct type");
      }
      my $found = 0;
      foreach (@{$typedef->{'enumeration'}}) {
        $found = 1 if $_ eq $data;
      }
      return $INVALID_ENUMERATION, 1 if not $found;
    }
    if(looks_like_number($data)) {
      return $INVALID_MIN_RANGE, 1 if defined($typedef->{'minInclusive'}) and $data < $typedef->{'minInclusive'};
      return $INVALID_MAX_RANGE, 1 if defined($typedef->{'maxInclusive'}) and $data > $typedef->{'maxInclusive'};
      return $INVALID_MIN_RANGE, 1 if defined($typedef->{'minExclusive'}) and $data <= $typedef->{'minExclusive'};
      return $INVALID_MAX_RANGE, 1 if defined($typedef->{'maxExclusive'}) and $data >= $typedef->{'maxExclusive'};
#      return $INVALID_FRACTION, 1 if defined($typedef->{'fractionDigits'}) and $data !~ /\.(\d{})$/;
    } elsif(defined($typedef->{'minInclusive'}) or defined($typedef->{'maxInclusive'}) or
      defined($typedef->{'minExclusive'}) or defined($typedef->{'maxExclusive'}) or
      defined($typedef->{'fractionDigits'})) {
      return $INVALID_NUMBER, 1;
    }
  } elsif(defined($definition->{'complexTypes'}->{$type})) {
    my $typedef = $definition->{'complexTypes'}->{$type};
    if(ref($data) eq "HASH") {
      return $self->_validate_elements( definition => $typedef, data => $data );
    } else {
      return $INVALID_COMPLEX, 1;
    }
  } else {
    croak("Validator Error: Can not find type definition '$type'");
    return $CRITICAL, 1;
  }
  
  return $NOERROR, 0;
}

=head2 $validator->_find_value( %p )

  Internal method for finding a value match (basic xpath)

=cut
sub _find_value
{
  my ($self, %p) = @_;
  # Remove root path, and stop localisation
  if($p{'path'} =~ s/^\///){ $p{'data'} = $self->{'data'}; }

  my @paths = split('/', $p{'path'});
  my $data  = $p{'data'};

  foreach my $path (@paths) {
    if(UNIVERSAL::isa($data, 'HASH')) {
      if(defined($data->{$path})) {
        $data = $data->{$path};
      } else {
        carp "Validator Error: Can't find nodes for '$p{'path'}' in _find_value\n" if $self->{'debug'};
      }
    } else {
      carp "Validator Error: Can't find nodes for '$p{'path'}' in _find_value\n" if $self->{'debug'};
    }
  }
  return $data;
}

=head2 $validator->_push_hash( $dest, $source )

  Internal method for copying a hash to another

=cut
sub _push_hash
{
  my($self, $dest, $source) = @_;

  foreach my $key (keys(%{$source})) {
    if(not $dest->{$key}) {
      $dest->{$key} = $source->{$key};
    }
  }
  return $dest;
}

=head2 $validator->_load_file( $file )

  Internal method for loading a file, must be valid perl syntax.
  Yep that's right, be bloody careful when loading from files.

=cut
sub _load_file {
  my ($self, $filename) = @_;

  open( VALIDATE, $filename );
    my $content = join('', <VALIDATE>);
  close( VALIDATE );

  my $structure = eval('{ '.$content.' }');
  croak("Validator Error! $@") if $@;
  return $structure;
}

=head1 AUTHOR

 I<Martin Owens> Copyright 2007, GPLv3

=cut
1;
