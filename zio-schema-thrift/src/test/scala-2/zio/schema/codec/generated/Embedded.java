/**
 * Autogenerated by Thrift Compiler (0.16.0)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
package zio.schema.codec.generated;

@SuppressWarnings({"cast", "rawtypes", "serial", "unchecked", "unused"})
@javax.annotation.Generated(value = "Autogenerated by Thrift Compiler (0.16.0)")
public class Embedded implements org.apache.thrift.TBase<Embedded, Embedded._Fields>, java.io.Serializable, Cloneable, Comparable<Embedded> {
  private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new org.apache.thrift.protocol.TStruct("Embedded");

  private static final org.apache.thrift.protocol.TField EMBEDDED_FIELD_DESC = new org.apache.thrift.protocol.TField("embedded", org.apache.thrift.protocol.TType.STRUCT, (short)1);

  private static final org.apache.thrift.scheme.SchemeFactory STANDARD_SCHEME_FACTORY = new EmbeddedStandardSchemeFactory();
  private static final org.apache.thrift.scheme.SchemeFactory TUPLE_SCHEME_FACTORY = new EmbeddedTupleSchemeFactory();

  public @org.apache.thrift.annotation.Nullable BasicInt embedded; // required

  /** The set of fields this struct contains, along with convenience methods for finding and manipulating them. */
  public enum _Fields implements org.apache.thrift.TFieldIdEnum {
    EMBEDDED((short)1, "embedded");

    private static final java.util.Map<java.lang.String, _Fields> byName = new java.util.HashMap<java.lang.String, _Fields>();

    static {
      for (_Fields field : java.util.EnumSet.allOf(_Fields.class)) {
        byName.put(field.getFieldName(), field);
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, or null if its not found.
     */
    @org.apache.thrift.annotation.Nullable
    public static _Fields findByThriftId(int fieldId) {
      switch(fieldId) {
        case 1: // EMBEDDED
          return EMBEDDED;
        default:
          return null;
      }
    }

    /**
     * Find the _Fields constant that matches fieldId, throwing an exception
     * if it is not found.
     */
    public static _Fields findByThriftIdOrThrow(int fieldId) {
      _Fields fields = findByThriftId(fieldId);
      if (fields == null) throw new java.lang.IllegalArgumentException("Field " + fieldId + " doesn't exist!");
      return fields;
    }

    /**
     * Find the _Fields constant that matches name, or null if its not found.
     */
    @org.apache.thrift.annotation.Nullable
    public static _Fields findByName(java.lang.String name) {
      return byName.get(name);
    }

    private final short _thriftId;
    private final java.lang.String _fieldName;

    _Fields(short thriftId, java.lang.String fieldName) {
      _thriftId = thriftId;
      _fieldName = fieldName;
    }

    public short getThriftFieldId() {
      return _thriftId;
    }

    public java.lang.String getFieldName() {
      return _fieldName;
    }
  }

  // isset id assignments
  public static final java.util.Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> metaDataMap;
  static {
    java.util.Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> tmpMap = new java.util.EnumMap<_Fields, org.apache.thrift.meta_data.FieldMetaData>(_Fields.class);
    tmpMap.put(_Fields.EMBEDDED, new org.apache.thrift.meta_data.FieldMetaData("embedded", org.apache.thrift.TFieldRequirementType.DEFAULT, 
        new org.apache.thrift.meta_data.StructMetaData(org.apache.thrift.protocol.TType.STRUCT, BasicInt.class)));
    metaDataMap = java.util.Collections.unmodifiableMap(tmpMap);
    org.apache.thrift.meta_data.FieldMetaData.addStructMetaDataMap(Embedded.class, metaDataMap);
  }

  public Embedded() {
  }

  public Embedded(
    BasicInt embedded)
  {
    this();
    this.embedded = embedded;
  }

  /**
   * Performs a deep copy on <i>other</i>.
   */
  public Embedded(Embedded other) {
    if (other.isSetEmbedded()) {
      this.embedded = new BasicInt(other.embedded);
    }
  }

  public Embedded deepCopy() {
    return new Embedded(this);
  }

  @Override
  public void clear() {
    this.embedded = null;
  }

  @org.apache.thrift.annotation.Nullable
  public BasicInt getEmbedded() {
    return this.embedded;
  }

  public Embedded setEmbedded(@org.apache.thrift.annotation.Nullable BasicInt embedded) {
    this.embedded = embedded;
    return this;
  }

  public void unsetEmbedded() {
    this.embedded = null;
  }

  /** Returns true if field embedded is set (has been assigned a value) and false otherwise */
  public boolean isSetEmbedded() {
    return this.embedded != null;
  }

  public void setEmbeddedIsSet(boolean value) {
    if (!value) {
      this.embedded = null;
    }
  }

  public void setFieldValue(_Fields field, @org.apache.thrift.annotation.Nullable java.lang.Object value) {
    switch (field) {
    case EMBEDDED:
      if (value == null) {
        unsetEmbedded();
      } else {
        setEmbedded((BasicInt)value);
      }
      break;

    }
  }

  @org.apache.thrift.annotation.Nullable
  public java.lang.Object getFieldValue(_Fields field) {
    switch (field) {
    case EMBEDDED:
      return getEmbedded();

    }
    throw new java.lang.IllegalStateException();
  }

  /** Returns true if field corresponding to fieldID is set (has been assigned a value) and false otherwise */
  public boolean isSet(_Fields field) {
    if (field == null) {
      throw new java.lang.IllegalArgumentException();
    }

    switch (field) {
    case EMBEDDED:
      return isSetEmbedded();
    }
    throw new java.lang.IllegalStateException();
  }

  @Override
  public boolean equals(java.lang.Object that) {
    if (that instanceof Embedded)
      return this.equals((Embedded)that);
    return false;
  }

  public boolean equals(Embedded that) {
    if (that == null)
      return false;
    if (this == that)
      return true;

    boolean this_present_embedded = true && this.isSetEmbedded();
    boolean that_present_embedded = true && that.isSetEmbedded();
    if (this_present_embedded || that_present_embedded) {
      if (!(this_present_embedded && that_present_embedded))
        return false;
      if (!this.embedded.equals(that.embedded))
        return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    int hashCode = 1;

    hashCode = hashCode * 8191 + ((isSetEmbedded()) ? 131071 : 524287);
    if (isSetEmbedded())
      hashCode = hashCode * 8191 + embedded.hashCode();

    return hashCode;
  }

  @Override
  public int compareTo(Embedded other) {
    if (!getClass().equals(other.getClass())) {
      return getClass().getName().compareTo(other.getClass().getName());
    }

    int lastComparison = 0;

    lastComparison = java.lang.Boolean.compare(isSetEmbedded(), other.isSetEmbedded());
    if (lastComparison != 0) {
      return lastComparison;
    }
    if (isSetEmbedded()) {
      lastComparison = org.apache.thrift.TBaseHelper.compareTo(this.embedded, other.embedded);
      if (lastComparison != 0) {
        return lastComparison;
      }
    }
    return 0;
  }

  @org.apache.thrift.annotation.Nullable
  public _Fields fieldForId(int fieldId) {
    return _Fields.findByThriftId(fieldId);
  }

  public void read(org.apache.thrift.protocol.TProtocol iprot) throws org.apache.thrift.TException {
    scheme(iprot).read(iprot, this);
  }

  public void write(org.apache.thrift.protocol.TProtocol oprot) throws org.apache.thrift.TException {
    scheme(oprot).write(oprot, this);
  }

  @Override
  public java.lang.String toString() {
    java.lang.StringBuilder sb = new java.lang.StringBuilder("Embedded(");
    boolean first = true;

    sb.append("embedded:");
    if (this.embedded == null) {
      sb.append("null");
    } else {
      sb.append(this.embedded);
    }
    first = false;
    sb.append(")");
    return sb.toString();
  }

  public void validate() throws org.apache.thrift.TException {
    // check for required fields
    // check for sub-struct validity
    if (embedded != null) {
      embedded.validate();
    }
  }

  private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
    try {
      write(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(out)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private void readObject(java.io.ObjectInputStream in) throws java.io.IOException, java.lang.ClassNotFoundException {
    try {
      read(new org.apache.thrift.protocol.TCompactProtocol(new org.apache.thrift.transport.TIOStreamTransport(in)));
    } catch (org.apache.thrift.TException te) {
      throw new java.io.IOException(te);
    }
  }

  private static class EmbeddedStandardSchemeFactory implements org.apache.thrift.scheme.SchemeFactory {
    public EmbeddedStandardScheme getScheme() {
      return new EmbeddedStandardScheme();
    }
  }

  private static class EmbeddedStandardScheme extends org.apache.thrift.scheme.StandardScheme<Embedded> {

    public void read(org.apache.thrift.protocol.TProtocol iprot, Embedded struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TField schemeField;
      iprot.readStructBegin();
      while (true)
      {
        schemeField = iprot.readFieldBegin();
        if (schemeField.type == org.apache.thrift.protocol.TType.STOP) { 
          break;
        }
        switch (schemeField.id) {
          case 1: // EMBEDDED
            if (schemeField.type == org.apache.thrift.protocol.TType.STRUCT) {
              struct.embedded = new BasicInt();
              struct.embedded.read(iprot);
              struct.setEmbeddedIsSet(true);
            } else { 
              org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
            }
            break;
          default:
            org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);
        }
        iprot.readFieldEnd();
      }
      iprot.readStructEnd();

      // check for required fields of primitive type, which can't be checked in the validate method
      struct.validate();
    }

    public void write(org.apache.thrift.protocol.TProtocol oprot, Embedded struct) throws org.apache.thrift.TException {
      struct.validate();

      oprot.writeStructBegin(STRUCT_DESC);
      if (struct.embedded != null) {
        oprot.writeFieldBegin(EMBEDDED_FIELD_DESC);
        struct.embedded.write(oprot);
        oprot.writeFieldEnd();
      }
      oprot.writeFieldStop();
      oprot.writeStructEnd();
    }

  }

  private static class EmbeddedTupleSchemeFactory implements org.apache.thrift.scheme.SchemeFactory {
    public EmbeddedTupleScheme getScheme() {
      return new EmbeddedTupleScheme();
    }
  }

  private static class EmbeddedTupleScheme extends org.apache.thrift.scheme.TupleScheme<Embedded> {

    @Override
    public void write(org.apache.thrift.protocol.TProtocol prot, Embedded struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TTupleProtocol oprot = (org.apache.thrift.protocol.TTupleProtocol) prot;
      java.util.BitSet optionals = new java.util.BitSet();
      if (struct.isSetEmbedded()) {
        optionals.set(0);
      }
      oprot.writeBitSet(optionals, 1);
      if (struct.isSetEmbedded()) {
        struct.embedded.write(oprot);
      }
    }

    @Override
    public void read(org.apache.thrift.protocol.TProtocol prot, Embedded struct) throws org.apache.thrift.TException {
      org.apache.thrift.protocol.TTupleProtocol iprot = (org.apache.thrift.protocol.TTupleProtocol) prot;
      java.util.BitSet incoming = iprot.readBitSet(1);
      if (incoming.get(0)) {
        struct.embedded = new BasicInt();
        struct.embedded.read(iprot);
        struct.setEmbeddedIsSet(true);
      }
    }
  }

  private static <S extends org.apache.thrift.scheme.IScheme> S scheme(org.apache.thrift.protocol.TProtocol proto) {
    return (org.apache.thrift.scheme.StandardScheme.class.equals(proto.getScheme()) ? STANDARD_SCHEME_FACTORY : TUPLE_SCHEME_FACTORY).getScheme();
  }
}
