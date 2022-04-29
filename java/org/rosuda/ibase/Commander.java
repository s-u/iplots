package org.rosuda.ibase;

/** Commander interface to pass action commands between objects
    @version $Id: Commander.java 454 2003-07-30 23:03:55Z starsoft $
 */
public interface Commander
{
    /** run command cmd issued by another object
	@param o origin of the command
	@param cmd command string 
	@return any object, the actual interpretation is up to the calling object
    */
    public Object run(Object o, String cmd);
};
